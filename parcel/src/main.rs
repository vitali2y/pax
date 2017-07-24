extern crate esparse;
extern crate crossbeam;
extern crate num_cpus;
extern crate notify;
extern crate json;
extern crate memchr;
extern crate base64;
#[macro_use]
extern crate matches;

use std::{env, process, io, fs, thread, time, iter};
use std::io::prelude::*;
use std::fmt::Write;
use std::path::{self, PathBuf, Path, Component};
use std::sync::mpsc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::collections::{HashMap, HashSet};
use std::any::Any;
use std::borrow::Cow;
use std::ffi::OsString;
use crossbeam::sync::SegQueue;
use notify::Watcher;

use esparse::lex;

const HEAD_JS: &'static str = include_str!("head.js");
const TAIL_JS: &'static str = include_str!("tail.js");

macro_rules! eat {
    (@collect $lexer:expr, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, _ => $else:expr $(,)*) => {
        match $lexer.here().tt {
            $($($p)|+ if $c => {
                $lexer.advance();
                $e
            })*
            _ => $else
        }
    };
    (@collect $lexer:expr, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, $($q:pat)|+ => $f:expr, $($t:tt)+) => {
        eat!(@collect $lexer, { $($($p)|+ if $c => $e ,)* $($q)|+ if true => $f, }, $($t)+)
    };
    (@collect $lexer:expr, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, $($q:pat)|+ if $d:expr => $f:expr, $($t:tt)+) => {
        eat!(@collect $lexer, { $($($p)|+ if $c => $e ,)* $($q)|+ if $d => $f, }, $($t)+)
    };
    ($lexer:expr, $($t:tt)+) => {
        eat!(@collect $lexer, {}, $($t)+)
    };
}

#[inline]
fn scan_for_require<'f, 's>(lex: &mut lex::Lexer<'f, 's>) -> Option<Cow<'s, str>> {
    loop {
        eat!(lex,
            // lex::Tt::Id(s) if s == "require" => eat!(lex,
            lex::Tt::Id("require") => eat!(lex,
                lex::Tt::Lparen => eat!(lex,
                    lex::Tt::StrLitSgl(s) |
                    lex::Tt::StrLitDbl(s) => eat!(lex,
                        lex::Tt::Rparen => {
                            // TODO handle error
                            return Some(lex::str_lit_value(s).unwrap())
                        },
                        _ => {
                            // panic!("weird require call {:?}", lex.here());
                        }
                    ),
                    _ => {
                        // panic!("dynamic require {:?}", lex.here())
                    },
                ),
                _ => {
                },
            ),
            lex::Tt::Eof => return None,
            _ => {
                lex.advance();
            },
        );
    }
}

#[derive(Debug)]
struct Writer<'a, 'b> {
    modules: HashMap<PathBuf, Module>,
    entry_point: &'a Path,
    map_output: &'b SourceMapOutput,
}

impl<'a, 'b> Writer<'a, 'b> {
    fn sorted_modules(&self) -> Vec<(&PathBuf, &Module)> {
        let mut modules = self.modules.iter().collect::<Vec<_>>();
        modules.sort_by(|&(ref f, _), &(ref g, _)| f.cmp(g));
        modules
    }

    fn write_to<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        w.write_all(HEAD_JS.as_bytes())?;
        // for (module, main) in self.mains {
        //     write!(w,
        //         "\n  Parcel.mains[{mod_path}] = {main_path}",
        //         mod_path = Self::js_path(&module),
        //         main_path = Self::js_path(&main),
        //     );
        // }

        for (file, info) in self.sorted_modules() {
            let id = Self::name_path(&file);
            let prefix = if matches!(file.extension(), Some(s) if s == "json") {
                "module.exports="
            } else {
                ""
            };
            let deps = Self::stringify_deps(&info.deps);
            let filename = Self::js_path(&file);

            write!(w,
                "\n  Parcel.files[{filename}] = {id}; {id}.deps = {deps}; {id}.filename = {filename}; function {id}(module, exports, require) {{{prefix}\n",
                filename = filename,
                id = id,
                deps = deps,
                prefix = prefix,
            )?;
            w.write_all(info.source.as_bytes())?;
            write!(w, "}}")?;
        }
        let main = Self::name_path(self.entry_point);
        write!(w,
            "\n  Parcel.main = {main}; Parcel.makeRequire(null)()\n  if (typeof module !== 'undefined') module.exports = Parcel.main.module && Parcel.main.module.exports\n",
            main = main,
        )?;
        w.write_all(TAIL_JS.as_bytes())?;
        match *self.map_output {
            SourceMapOutput::Suppressed => {}
            SourceMapOutput::Inline => {
                let mut map = Vec::new();
                self.write_map_to(&mut map)?;
                write!(w,
                    "//# sourceMappingURL=data:application/json;charset=utf-8;base64,{data}\n",
                    data = base64::encode(&map),
                )?;
            }
            SourceMapOutput::File(ref path) => {
                // TODO handle error
                let relative = path.relative_from(self.entry_point.parent().unwrap());
                let map = relative.as_ref().unwrap_or(path);
                write!(w,
                    "//# sourceMappingURL={map}\n",
                    map = map.display(),
                )?;
            }
        }
        Ok(())
    }

    fn write_map_to<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        // TODO this is borderline unmaintainable, but constructing a JsonValue is *really* wasteful

        let modules = self.sorted_modules();
        let dir = self.entry_point.parent().unwrap();

        w.write_all(r#"{"version":3,"file":"","sourceRoot":"","sources":["#.as_bytes())?;

        let mut comma = false;
        for &(f, _) in &modules {
            if comma {
                w.write_all(b",")?;
            }
            let relative = f.relative_from(dir);
            let path = relative.as_ref().unwrap_or(f);
            let string = path.to_string_lossy();
            write_json_string(w, string.as_ref())?;
            comma = true;
        }
        w.write_all(r#"],"sourcesContent":["#.as_bytes())?;

        let mut comma = false;
        for &(_, module) in &modules {
            if comma {
                w.write_all(b",")?;
            }
            write_json_string(w, &module.source)?;
            comma = true;
        }
        w.write_all(r#"],"names":[],"mappings":""#.as_bytes())?;

        let prefix_len = count_lines(HEAD_JS); /*+ this.mains.size*/
        for _ in 0..prefix_len {
            w.write_all(b";")?;
        }

        let mut line = 0;
        let mut buf = [0u8; 13];
        for (index, &(_, module)) in modules.iter().enumerate() {
            w.write_all(b";")?;
            for i in 0..count_lines(&module.source) {
                w.write_all(b"A")?;
                if i == 0 {
                    if index == 0 {
                        w.write_all(b"AAA")?;
                    } else {
                        w.write_all(b"C")?;
                        w.write_all(vlq(&mut buf, -line))?;
                        w.write_all(b"A")?;
                    }
                    line = 0;
                } else {
                    w.write_all(b"ACA")?;
                    line += 1;
                }
                w.write_all(b";")?;
            }
        }
        for _ in 0..2 + count_lines(TAIL_JS) + 1 - 1 - 1 {
            w.write_all(b";")?;
        }
        w.write_all(r#""}"#.as_bytes())?;
        Ok(())
    }

    fn stringify_deps(deps: &HashMap<String, Resolved>) -> String {
        let mut result = "{".to_owned();
        let mut comma = false;
        for (name, resolved) in deps {
            match *resolved {
                Resolved::Core => {}
                Resolved::Normal(ref path) => {
                    if comma {
                        result.push(',');
                    }
                    result.push_str(&json::stringify(json::JsonValue::from(name.as_ref())));
                    result.push(':');
                    Self::write_name_path(path, &mut result);
                    comma = true;
                }
            }
        }
        result.push('}');
        result
    }

    #[cfg(target_os = "windows")]
    fn js_path(path: &Path) -> String {
        // TODO untested
        let string = path.to_string_lossy();
        let replaced = string.as_ref().replace('\\', "/");
        json::stringify(json::JsonValue::from(replaced))
    }

    #[cfg(not(target_os = "windows"))]
    fn js_path(path: &Path) -> String {
        let string = path.to_string_lossy().into_owned();
        json::stringify(json::JsonValue::from(string))
    }

    fn name_path(path: &Path) -> String {
        let mut result = String::new();
        Self::write_name_path(path, &mut result);
        result
    }
    fn write_name_path(path: &Path, result: &mut String) {
        let string = path.to_string_lossy();
        // let slice = string.as_ref();
        let bytes = string.as_bytes();

        result.push_str("file_");
        for &b in bytes {
            match b {
                b'_' | b'a'...b'z' | b'A'...b'Z' => {
                    result.push(b as char);
                }
                _ => {
                    write!(result, "${:02x}", b).unwrap();
                }
            }
        }

        // let mut last_pos = 0;
        // for pos in bytes.match_indices(|&b| {
        //     !matches!(b as char, '_' | 'a'...'z' | 'A'...'Z')
        // }) {
        //     result.push_str(slice[last_pos..pos]);
        //     write!(result, "${:02x}", bytes[pos]);
        //     last_pos = pos + 1;
        // }
    }
}

fn count_lines(source: &str) -> usize {
    // TODO non-ASCII line terminators?
    1 + memchr::Memchr::new(b'\n', source.as_bytes()).count()
}

const QU: u8 = b'"';
const BS: u8 = b'\\';
const BB: u8 = b'b';
const TT: u8 = b't';
const NN: u8 = b'n';
const FF: u8 = b'f';
const RR: u8 = b'r';
const UU: u8 = b'u';
const __: u8 = 0;

static ESCAPED: [u8; 256] = [
// 0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
  UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
  UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
  __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
  __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
  __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
];

#[inline(never)]
#[cold]
fn write_json_string_complex<W: io::Write>(w: &mut W, string: &str, mut start: usize) -> io::Result<()> {
    w.write_all(string[..start].as_bytes())?;

    for (index, ch) in string.bytes().enumerate().skip(start) {
        let escape = ESCAPED[ch as usize];
        if escape > 0 {
            w.write_all(string[start..index].as_bytes())?;
            w.write_all(&[b'\\', escape])?;
            start = index + 1;
        }
        if escape == b'u' {
            write!(w, "{:04x}", ch)?;
        }
    }
    w.write_all(string[start..].as_bytes())?;
    w.write_all(&[b'"'])?;
    Ok(())
}

#[inline]
fn write_json_string<W: io::Write>(w: &mut W, string: &str) -> io::Result<()> {
    w.write_all(&[b'"'])?;

    for (index, ch) in string.bytes().enumerate() {
        if ESCAPED[ch as usize] > 0 {
            return write_json_string_complex(w, string, index)
        }
    }

    w.write_all(string.as_bytes())?;
    w.write_all(&[b'"'])?;
    Ok(())
}

// fn write_vlq<W: io::Write>(w: &mut W, n: isize) -> io::Result<()> {
//     let sign = n < 0;
//     let n = if sign { -n } else { n } as usize;
//     let y = (n & 0xf) << 1 | sign as usize;
//     let r = n >> 4;
//     while r > 0 {
//         y |= 0x20;
//         w.write_all(&[B64[y]])?;
//         y = r & 0x1f;
//         r >>= 5;
//     }
//     w.write_all(&[B64[y]])
// }
fn vlq(buf: &mut [u8; 13], n: isize) -> &[u8] {
    let sign = n < 0;
    let n = if sign { -n } else { n } as usize;
    let mut y = (n & 0xf) << 1 | sign as usize;
    let mut r = n >> 4;
    let mut l = 0;
    while r > 0 {
        y |= 0x20;
        buf[l] = B64[y];
        y = r & 0x1f;
        r >>= 5;
        l += 1;
    }
    buf[l] = B64[y];
    &buf[0..l+1]
}
const B64: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";


#[derive(Debug, Clone)]
struct Worker {
    tx: mpsc::Sender<Result<WorkDone, CliError>>,
    queue: Arc<SegQueue<Work>>,
    quit: Arc<AtomicBool>,
}

// TODO use references
#[derive(Debug)]
enum Work {
    Resolve { context: PathBuf, name: String },
    Include { module: PathBuf },
}
#[derive(Debug)]
enum WorkDone {
    Resolve { context: PathBuf, name: String, resolved: Resolved },
    Include { module: PathBuf, info: ModuleInfo },
}
#[derive(Debug)]
enum ModuleState {
    Loading,
    Loaded(Module),
}
#[derive(Debug)]
struct Module {
    source: String,
    deps: HashMap<String, Resolved>,
}
#[derive(Debug)]
struct ModuleInfo {
    source: String,
    deps: Vec<String>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum Resolved {
    Core,
    // CoreWithSubst(PathBuf),
    Normal(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceMapOutput {
    Suppressed,
    Inline,
    File(PathBuf),
}

impl ModuleState {
    fn expect(self, message: &str) -> Module {
        match self {
            ModuleState::Loading => panic!("{}", message),
            ModuleState::Loaded(module) => module,
        }
    }
    fn unwrap(self) -> Module {
        self.expect("unwrapped ModuleState that was still loading")
    }
}

fn bundle(entry_point: &Path, output: &str, map_output: &SourceMapOutput) -> Result<HashMap<PathBuf, Module>, CliError> {
    let mut pending = 0;
    let thread_count = num_cpus::get();
    let (tx, rx) = mpsc::channel();
    let worker = Worker {
        tx,
        quit: Arc::new(AtomicBool::new(false)),
        queue: Arc::new(SegQueue::new()),
    };

    let mut modules = HashMap::<PathBuf, ModuleState>::new();

    worker.add_work(Work::Include { module: entry_point.to_owned() });
    pending += 1;
    modules.insert(entry_point.to_owned(), ModuleState::Loading);

    let children: Vec<_> = (0..thread_count).map(|_| {
        let worker = worker.clone();
        thread::spawn(move || worker.run())
    }).collect();

    loop {
        let work_done = match rx.recv() {
            Ok(work_done) => work_done,
            Err(_) => break,
        };
        // println!("{:?}", work_done);
        let work_done = match work_done {
            Err(error) => {
                return Err(error)
            }
            Ok(work_done) => {
                pending -= 1;
                work_done
            }
        };
        match work_done {
            WorkDone::Resolve { context, name, resolved } => {
                match *modules.get_mut(&context).unwrap() {
                    ModuleState::Loading => unreachable!(),
                    ModuleState::Loaded(Module { ref mut deps, .. }) => {
                        deps.insert(name, resolved.clone());
                    }
                }
                match resolved {
                    Resolved::Core => {}
                    Resolved::Normal(module) => {
                        modules.entry(module.clone()).or_insert_with(|| {
                            worker.add_work(Work::Include { module });
                            pending += 1;
                            ModuleState::Loading
                        });
                    }
                }
            }
            WorkDone::Include { module, info } => {
                let old = modules.insert(module.clone(), ModuleState::Loaded(Module {
                    source: info.source,
                    deps: HashMap::new(),
                }));
                debug_assert_matches!(old, Some(ModuleState::Loading));
                for dep in info.deps {
                    worker.add_work(Work::Resolve {
                        context: module.clone(),
                        name: dep,
                    });
                    pending += 1;
                }
            }
        }
        if pending == 0 {
            break
        }
    }

    worker.quit.store(true, Ordering::Relaxed);

    for child in children {
        child.join()?;
    }

    let writer = Writer {
        modules: modules.into_iter()
        .map(|(k, ms)| (k, ms.unwrap()))
        .collect(),
        entry_point,
        map_output,
    };

    match &*output {
        "-" => {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            writer.write_to(&mut handle)?;
        }
        _ => {
            let file = fs::File::create(&output)?;
            let mut buf_writer = io::BufWriter::new(file);
            writer.write_to(&mut buf_writer)?;
        }
    }
    match *map_output {
        SourceMapOutput::Suppressed => {}
        SourceMapOutput::Inline => {
            // handled in Writer::write_to()
        }
        SourceMapOutput::File(ref path) => {
            let file = fs::File::create(path)?;
            let mut buf_writer = io::BufWriter::new(file);
            writer.write_map_to(&mut buf_writer)?;
        }
    }
    // println!("entry point: {:?}", entry_point);
    // println!("{:#?}", modules);

    Ok(writer.modules)
}

fn run() -> Result<(), CliError> {
    let mut input = None;
    let mut output = None;
    let mut map = None;
    let mut map_inline = false;
    let mut no_map = false;
    let mut watch = false;

    let mut iter = env::args().skip(1);
    while let Some(arg) = iter.next() {
        match arg.as_ref() {
            "-h" | "--help" => return Err(CliError::Help),
            "-w" | "--watch" => watch = true,
            "-I" | "--map-inline" => map_inline = true,
            "-M" | "--no-map" => no_map = true,
            "-m" | "--map" => {
                if map.is_some() {
                    return Err(CliError::DuplicateOption(arg))
                }
                map = Some(iter.next().ok_or(CliError::MissingOptionValue(arg))?)
            }
            "-i" | "--input" => {
                if input.is_some() {
                    return Err(CliError::DuplicateOption(arg))
                }
                input = Some(iter.next().ok_or(CliError::MissingOptionValue(arg))?)
            }
            "-o" | "--output" => {
                if output.is_some() {
                    return Err(CliError::DuplicateOption(arg))
                }
                output = Some(iter.next().ok_or(CliError::MissingOptionValue(arg))?)
            }
            _ => {
                if arg.starts_with("-") {
                    return Err(CliError::UnknownOption(arg))
                }
                if input.is_none() {
                    input = Some(arg)
                } else if output.is_none() {
                    output = Some(arg)
                } else {
                    return Err(CliError::UnexpectedArg(arg))
                }
            }
        }
    }

    if map_inline as u8 + no_map as u8 + map.is_some() as u8 > 1 {
        return Err(CliError::BadUsage("--map-inline, --map <file>, and --no-map are mutually exclusive"))
    }

    let input = input.ok_or(CliError::MissingFileName)?;
    let input_dir = env::current_dir()?;
    let output = output.unwrap_or("-".to_owned());

    let map_output = if map_inline {
        SourceMapOutput::Inline
    } else if no_map {
        SourceMapOutput::Suppressed
    } else {
        match map {
            Some(path) => {
                SourceMapOutput::File(PathBuf::from(path))
            }
            None => {
                if output == "-" {
                    SourceMapOutput::Suppressed
                } else {
                    let mut buf = OsString::from(&output);
                    buf.push(".map");
                    SourceMapOutput::File(PathBuf::from(buf))
                }
            }
        }
    };

    // println!("{} => {} (watching: {:?})", input, output, watch);
    // println!();

    let entry_point = match Worker::resolve(&input_dir, &input, true)? {
        Resolved::Normal(resolved) => resolved,
        _ => panic!("non-normal entry point module"),
    };

    if watch {
        let (tx, rx) = mpsc::channel();
        let debounce_dur = time::Duration::from_millis(5);
        let mut watcher = notify::raw_watcher(tx.clone())?;

        let mut modules = bundle(&entry_point, &output, &map_output)?;
        for path in modules.keys() {
            watcher.watch(path, notify::RecursiveMode::NonRecursive)?;
        }

        eprintln!("ready");
        loop {
            let first_event = rx.recv().expect("notify::watcher disconnected");
            thread::sleep(debounce_dur);
            for event in iter::once(first_event).chain(rx.try_iter()) {
                let _op = event.op?;
                // match event {
                //     notify::DebouncedEvent::Error(error, _) => return Err(From::from(error)),
                //     _ => {}
                // }
            }

            let start_inst = time::Instant::now();
            let new_modules = bundle(&entry_point, &output, &map_output)?;

            let elapsed = start_inst.elapsed();
            let ms = elapsed.as_secs() * 1_000 + (elapsed.subsec_nanos() / 1_000_000) as u64;
            eprintln!("generate {output} in {ms} ms",
                output = output,
                ms = ms);

            {
                let mut to_unwatch = modules.keys().collect::<HashSet<_>>();
                let mut to_watch = new_modules.keys().collect::<HashSet<_>>();
                for path in modules.keys() {
                    to_watch.remove(&path);
                }
                for path in new_modules.keys() {
                    to_unwatch.remove(&path);
                }
                for path in to_watch {
                    watcher.watch(path, notify::RecursiveMode::NonRecursive)?;
                }
                for path in to_unwatch {
                    watcher.unwatch(path)?;
                }
            }
            modules = new_modules;
        }
    } else {
        bundle(&entry_point, &output, &map_output).map(|_| ())
    }
}

const APP_NAME: &'static str = env!("CARGO_PKG_NAME");
const APP_VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn print_usage() {
    println!("\
usage: {0} [options] <input> [output]
       {0} [-h | --help]
", APP_NAME);
}

fn print_help() {
        println!("\
{0} v{1}

usage: {0} [options] <input> [output]
       {0} [-h | --help]

options:
    -i, --input <input>
        use <input> as the main module

    -o, --output <output>
        write bundle to <output> and source map to <output>.map
        default '-' for stdout

    -m, --map <map>
        output source map to <map>

    -I, --map-inline
        output source map inline as data URI

    -M, --no-map
        suppress source map output when it would normally be implied

    -w, --watch
        watch for changes to <input> and its dependencies

    -h, --help
        print this message
", APP_NAME, APP_VERSION);
}

#[derive(Debug)]
enum CliError {
    Help,
    MissingFileName,
    DuplicateOption(String),
    MissingOptionValue(String),
    UnknownOption(String),
    UnexpectedArg(String),
    BadUsage(&'static str),

    RequireRoot { path: PathBuf },
    EmptyModuleName { context: PathBuf },
    ModuleNotFound { context: PathBuf, name: String },

    Io(io::Error),
    Json(json::Error),
    Notify(notify::Error),
    Box(Box<Any + Send + 'static>),
}
impl From<io::Error> for CliError {
    fn from(inner: io::Error) -> CliError {
        CliError::Io(inner)
    }
}
impl From<json::Error> for CliError {
    fn from(inner: json::Error) -> CliError {
        CliError::Json(inner)
    }
}
impl From<notify::Error> for CliError {
    fn from(inner: notify::Error) -> CliError {
        CliError::Notify(inner)
    }
}
impl From<Box<Any + Send + 'static>> for CliError {
    fn from(inner: Box<Any + Send + 'static>) -> CliError {
        CliError::Box(inner)
    }
}

fn main() {
    process::exit(match run() {
        Ok(_) => 0,
        Err(kind) => {
            match kind {
                CliError::Help => print_help(),
                CliError::MissingFileName => print_usage(),
                CliError::DuplicateOption(opt) => println!("{}: option {} specified more than once", APP_NAME, opt),
                CliError::MissingOptionValue(opt) => println!("{}: missing value for option {}", APP_NAME, opt),
                CliError::UnknownOption(opt) => println!("{}: unknown option {}", APP_NAME, opt),
                CliError::UnexpectedArg(arg) => println!("{}: unexpected argument {}", APP_NAME, arg),
                CliError::BadUsage(arg) => println!("{}: {}", APP_NAME, arg),

                CliError::RequireRoot { path } => println!("{}: require of root path {}", APP_NAME, path.display()), // TODO in what module
                CliError::EmptyModuleName { context } => println!("{}: require('') in {}", APP_NAME, context.display()),
                CliError::ModuleNotFound { context, name } => println!("{}: module '{}' not found in {}", APP_NAME, name, context.display()),

                CliError::Io(inner) => println!("{}: {}", APP_NAME, inner),
                CliError::Json(inner) => println!("{}: {}", APP_NAME, inner),
                CliError::Notify(inner) => println!("{}: {}", APP_NAME, inner),
                CliError::Box(inner) => println!("{}: {:?}", APP_NAME, inner),
            }
            1
        }
    })
}

trait PathBufExt {
    fn append_resolving<P: AsRef<Path> + ?Sized>(&mut self, more: &P);
}
impl PathBufExt for PathBuf {
    fn append_resolving<P: AsRef<Path> + ?Sized>(&mut self, more: &P) {
        for c in more.as_ref().components() {
            match c {
                Component::Prefix(prefix) => {
                    *self = PathBuf::from(prefix.as_os_str().to_owned());
                }
                Component::RootDir => {
                    self.push(path::MAIN_SEPARATOR.to_string());
                    // while self.pop() {}
                },
                Component::CurDir => {}
                Component::ParentDir => {
                    self.pop();
                }
                Component::Normal(part) => {
                    self.push(part);
                }
            }
        }
    }
}

trait PathExt {
    fn relative_from<P: AsRef<Path> + ?Sized>(&self, base: &P) -> Option<PathBuf>;
}
impl PathExt for Path {
    fn relative_from<P: AsRef<Path> + ?Sized>(&self, base: &P) -> Option<PathBuf> {
        let base = base.as_ref();
        if self.is_absolute() != base.is_absolute() {
            if self.is_absolute() {
                Some(PathBuf::from(self))
            } else {
                None
            }
        } else {
            let mut ita = self.components();
            let mut itb = base.components();
            let mut comps: Vec<Component> = vec![];
            loop {
                match (ita.next(), itb.next()) {
                    (None, None) => break,
                    (Some(a), None) => {
                        comps.push(a);
                        comps.extend(ita.by_ref());
                        break;
                    }
                    (None, _) => comps.push(Component::ParentDir),
                    (Some(a), Some(b)) if comps.is_empty() && a == b => (),
                    (Some(a), Some(b)) if b == Component::CurDir => comps.push(a),
                    (Some(_), Some(b)) if b == Component::ParentDir => return None,
                    (Some(a), Some(_)) => {
                        comps.push(Component::ParentDir);
                        for _ in itb {
                            comps.push(Component::ParentDir);
                        }
                        comps.push(a);
                        comps.extend(ita.by_ref());
                        break;
                    }
                }
            }
            Some(comps.iter().map(|c| c.as_os_str()).collect())
        }
    }
}

impl Worker {
    fn run(mut self) {
        while let Some(work) = self.get_work() {
            self.tx.send(match work {
                Work::Resolve { context, name } => {
                    Self::resolve(&context, &name, false)
                    .map(|resolved| WorkDone::Resolve {
                        context,
                        name,
                        resolved,
                    })
                }
                Work::Include { module } => {
                    Self::include(&module)
                    .map(|info| WorkDone::Include {
                        module,
                        info,
                    })
                }
            }).unwrap();
        }
    }

    fn resolve(context: &Path, name: &str, is_main: bool) -> Result<Resolved, CliError> {
        // match name.chars().next() {
        match name.as_bytes().get(0) {
            None => Err(CliError::EmptyModuleName {
                context: context.to_owned()
            }),
            Some(&b'/') => {
                Ok(Resolved::Normal(
                    Self::resolve_path_or_module(PathBuf::from(name))?.ok_or_else(|| {
                        CliError::ModuleNotFound {
                            context: context.to_owned(),
                            name: name.to_owned(),
                        }
                    })?,
                ))
            }
            Some(&b'.') => {
                let mut dir = context.to_owned();
                if !is_main {
                    let did_pop = dir.pop(); // to directory
                    debug_assert!(did_pop);
                }
                dir.append_resolving(Path::new(name));
                Ok(Resolved::Normal(
                    Self::resolve_path_or_module(dir)?.ok_or_else(|| {
                        CliError::ModuleNotFound {
                            context: context.to_owned(),
                            name: name.to_owned(),
                        }
                    })?,
                ))
            }
            // TODO support unix-style absolute paths?
            // TODO absolute paths on windows
            _ => {
                if is_main {
                    panic!("entry point must be a file")
                }

                match name {
                    "assert" | "buffer" | "child_process" | "cluster" | "crypto" | "dgram" | "dns" | "domain" | "events" | "fs" | "http" | "https" | "net" | "os" | "path" | "punycode" | "querystring" | "readline" | "stream" | "string_decoder" | "tls" | "tty" | "url" | "util" | "v8" | "vm" | "zlib" => return Ok(Resolved::Core),
                    _ => {}
                }

                let mut suffix = PathBuf::from("node_modules");
                suffix.push(name);

                let mut dir = context.to_owned();
                while dir.pop() {
                    match dir.file_name() {
                        Some(s) if s == "node_modules" => continue,
                        _ => {}
                    }
                    let new_path = dir.join(&suffix);
                    match Self::resolve_path_or_module(new_path)? {
                        Some(result) => return Ok(Resolved::Normal(result)),
                        None => {}
                    };
                }

                Err(CliError::ModuleNotFound {
                    context: context.to_owned(),
                    name: name.to_owned(),
                })
            }
        }
    }
    fn resolve_path_or_module(mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
        path.push("package.json");
        let result = fs::File::open(&path);
        path.pop();
        match result {
            Ok(file) => {
                let mut buf_reader = io::BufReader::new(file);
                let mut string = String::new();
                buf_reader.read_to_string(&mut string)?;

                let result = json::parse(&string)?;
                match result["main"].as_str() {
                    None => {}
                    Some(main) => {
                        path.append_resolving(main);
                    }
                }
            },
            Err(_error) => {
                // if error.kind() != io::ErrorKind::NotFound {
                //     return Err(From::from(error))
                // }
            }
        }
        Self::resolve_path(path)
    }

    fn resolve_path(mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
        // println!("resolve_path {}", path.display());
        if path.is_file() {
            return Ok(Some(path))
        }

        let file_name = path.file_name().ok_or_else(|| CliError::RequireRoot {
            path: path.clone(),
        })?.to_owned();

        // <path>.js
        let mut new_file_name = file_name.to_owned();
        new_file_name.push(".js");
        path.set_file_name(&new_file_name);
        if path.is_file() {
            return Ok(Some(path))
        }

        // <path>/index.js
        path.set_file_name(&file_name);
        path.push("index.js");
        if path.is_file() {
            return Ok(Some(path))
        }
        path.pop();

        // <path>.json
        new_file_name.push("on"); // .js|on
        path.set_file_name(&new_file_name);
        if path.is_file() {
            return Ok(Some(path))
        }

        // <path>/index.json
        path.set_file_name(&file_name);
        path.push("index.json");
        if path.is_file() {
            return Ok(Some(path))
        }
        // path.pop();

        Ok(None)
    }

    fn include(module: &Path) -> Result<ModuleInfo, CliError> {
        let mut source = String::new();
        let file = fs::File::open(module)?;
        let mut buf_reader = io::BufReader::new(file);
        buf_reader.read_to_string(&mut source)?;

        let deps = {
            let mut deps = HashSet::new();
            let path_string = module.to_string_lossy();

            // module.to_str().ok_or("<path with invalid utf-8>")
            let mut lexer = lex::Lexer::new(path_string.as_ref(), &source);
            while let Some(path) = scan_for_require(&mut lexer) {
                deps.insert(path);
            }

            deps.into_iter()
            .map(|s| s.into_owned())
            .collect()
        };

        Ok(ModuleInfo {
            source,
            deps,
        })
    }

    fn add_work(&self, work: Work) {
        self.queue.push(work);
    }

    fn get_work(&mut self) -> Option<Work> {
        loop {
            match self.queue.try_pop() {
                Some(work) => return Some(work),
                None => {
                    if self.quit.load(Ordering::Relaxed) {
                        return None
                    } else {
                        thread::yield_now();
                        // thread::sleep(time::Duration::from_millis(1));
                    }
                }
            }
        }
    }
}
