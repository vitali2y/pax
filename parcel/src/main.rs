#![cfg_attr(test, feature(test))]

#[macro_use]
extern crate esparse;
extern crate crossbeam;
extern crate num_cpus;
extern crate notify;
extern crate json;
extern crate memchr;
extern crate base64;
#[macro_use]
extern crate matches;

use std::{env, process, io, fs, thread, time, iter, fmt};
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
use esparse::lex::{self, Tt};

mod es6;

const HEAD_JS: &str = include_str!("head.js");
const TAIL_JS: &str = include_str!("tail.js");

fn cjs_parse_deps<'f, 's>(lex: &mut lex::Lexer<'f, 's>) -> Result<HashSet<Cow<'s, str>>, CliError> {
    // TODO should we panic on dynamic requires?
    let mut deps = HashSet::new();
    loop {
        eat!(lex,
            // Tt::Id(s) if s == "require" => eat!(lex,
            Tt::Id("require") => eat!(lex,
                Tt::Lparen => eat!(lex,
                    Tt::StrLitSgl(s) |
                    Tt::StrLitDbl(s) => eat!(lex,
                        Tt::Rparen => {
                            deps.insert(lex::str_lit_value(s)?);
                        },
                        _ => {},
                    ),
                    _ => {},
                ),
                // Tt::Dot => eat!(lex,
                //     Tt::Id("resolve") => eat!(lex,
                //         Tt::Lparen => eat!(lex,
                //             Tt::StrLitSgl(s) |
                //             Tt::StrLitDbl(s) => eat!(lex,
                //                 Tt::Rparen => {
                //                     // TODO handle error
                //                     return Some(lex::str_lit_value(s).unwrap())
                //                 },
                //                 _ => {},
                //             ),
                //             _ => {},
                //         ),
                //         _ => {},
                //     ),
                //     _ => {},
                // ),
                _ => {},
            ),
            Tt::Eof => return Ok(deps),
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
    map_output: &'b SourceMapOutput<'b>,
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
            let deps = Self::stringify_deps(&info.deps);
            let filename = Self::js_path(&file);

            write!(w,
                "\n  Parcel.files[{filename}] = {id}; {id}.deps = {deps}; {id}.filename = {filename}; function {id}(module, exports, require) {{\n",
                filename = filename,
                id = id,
                deps = deps,
            )?;
            if !info.source.prefix.is_empty() {
                w.write_all(info.source.prefix.as_bytes())?;
                w.write_all(b"\n")?;
            }
            w.write_all(info.source.body.as_bytes())?;
            if !matches!(info.source.body.chars().last(), None | Some('\n') | Some('\r') | Some('\u{2028}') | Some('\u{2029}')) {
                w.write_all(b"\n")?;
            }
            if !info.source.suffix.is_empty() {
                w.write_all(info.source.suffix.as_bytes())?;
            }
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
            SourceMapOutput::File(ref path, output_file) => {
                // TODO handle error
                let relative = path.relative_from(output_file.parent().unwrap());
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

        w.write_all(br#"{"version":3,"file":"","sourceRoot":"","sources":["#)?;

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
        w.write_all(br#"],"sourcesContent":["#)?;

        let mut comma = false;
        for &(_, module) in &modules {
            if comma {
                w.write_all(b",")?;
            }
            write_json_string(w, module.source.original.as_ref().unwrap_or_else(|| &module.source.body))?;
            comma = true;
        }
        w.write_all(br#"],"names":[],"mappings":""#)?;

        let prefix_len = count_lines(HEAD_JS); /*+ this.mains.size*/
        for _ in 0..prefix_len {
            w.write_all(b";")?;
        }

        let mut line = 0;
        let mut buf = [0u8; 13];
        for (index, &(_, module)) in modules.iter().enumerate() {
            w.write_all(b";")?;
            if !module.source.prefix.is_empty() {
                for _ in 0..count_lines(&module.source.prefix) {
                    w.write_all(b";")?;
                }
            }
            for i in 0..count_lines(&module.source.body) {
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
            if !matches!(module.source.body.chars().last(), None | Some('\n') | Some('\r') | Some('\u{2028}') | Some('\u{2029}')) {
                w.write_all(b";")?;
            }
            for _ in 0..count_lines(&module.source.suffix)-1 {
                w.write_all(b";")?;
            }
        }
        for _ in 0..2 + count_lines(TAIL_JS) + 1 - 1 - 1 {
            w.write_all(b";")?;
        }
        w.write_all(br#""}"#)?;
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
    input_options: InputOptions,
    queue: Arc<SegQueue<Work>>,
    quit: Arc<AtomicBool>,
}

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
pub struct Module {
    pub source: Source,
    pub deps: HashMap<String, Resolved>,
}
#[derive(Debug)]
struct ModuleInfo {
    source: Source,
    deps: Vec<String>,
}
#[derive(Debug)]
pub struct Source {
    pub prefix: String,
    pub body: String,
    pub suffix: String,
    pub original: Option<String>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolved {
    Core,
    // CoreWithSubst(PathBuf),
    Normal(PathBuf),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct InputOptions {
    pub es6_syntax: bool,
    pub es6_syntax_everywhere: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceMapOutput<'a> {
    Suppressed,
    Inline,
    File(PathBuf, &'a Path),
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

pub fn bundle(entry_point: &Path, input_options: InputOptions, output: &str, map_output: &SourceMapOutput) -> Result<HashMap<PathBuf, Module>, CliError> {
    let mut pending = 0;
    let thread_count = num_cpus::get();
    let (tx, rx) = mpsc::channel();
    let worker = Worker {
        tx,
        input_options,
        quit: Arc::new(AtomicBool::new(false)),
        queue: Arc::new(SegQueue::new()),
    };

    // TODO: context.require('…')
    // TODO: watch for missing files on error?

    let mut modules = HashMap::<PathBuf, ModuleState>::new();

    worker.add_work(Work::Include { module: entry_point.to_owned() });
    pending += 1;
    modules.insert(entry_point.to_owned(), ModuleState::Loading);

    let children: Vec<_> = (0..thread_count).map(|_| {
        let worker = worker.clone();
        thread::spawn(move || worker.run())
    }).collect();
    // let children: Vec<_> = (0..thread_count).map(|n| {
    //     let worker = worker.clone();
    //     thread::Builder::new().name(format!("worker #{}", n + 1)).spawn(move || worker.run()).unwrap()
    // }).collect();

    while let Ok(work_done) = rx.recv() {
        // eprintln!("{:?}", work_done);
        let work_done = match work_done {
            Err(error) => {
                worker.quit.store(true, Ordering::Relaxed);
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
            let output = Path::new(output);
            if let Some(parent) = output.parent() {
                fs::create_dir_all(parent)?;
            }
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
        SourceMapOutput::File(ref path, _) => {
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            let file = fs::File::create(path)?;
            let mut buf_writer = io::BufWriter::new(file);
            writer.write_map_to(&mut buf_writer)?;
        }
    }
    // println!("entry point: {:?}", entry_point);
    // println!("{:#?}", modules);

    Ok(writer.modules)
}

fn expand_arg(arg: String) -> ExpandArg {
    ExpandArg {
        arg: Some(arg),
        state: ExpandArgState::Start,
    }
}

#[derive(Debug)]
struct ExpandArg {
    arg: Option<String>,
    state: ExpandArgState,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpandArgState {
    Start,
    ShortOption(usize),
    Done,
}

impl Iterator for ExpandArg {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.state {
                ExpandArgState::Start => {
                    let is_short = {
                        let arg = self.arg.as_ref().unwrap().as_bytes();
                        arg.len() >= 2 && arg[0] == b'-' && arg[1] != b'-'
                    };
                    if is_short {
                        self.state = ExpandArgState::ShortOption(1);
                    } else {
                        self.state = ExpandArgState::Done;
                        return self.arg.take()
                    }
                }
                ExpandArgState::ShortOption(n) => {
                    let mut indices = self.arg.as_ref().unwrap()[n..].char_indices();
                    match indices.next() {
                        Some((_, c)) => {
                            self.state = match indices.next() {
                                Some((m, _)) => ExpandArgState::ShortOption(n + m),
                                None => ExpandArgState::Done,
                            };
                            return Some(format!("-{}", c))
                        }
                        None => unreachable!(),
                    }
                }
                ExpandArgState::Done => return None,
            }
        }
    }
}
// impl iter::FusedIterator for ExpandArg {}

fn run() -> Result<(), CliError> {
    let entry_inst = time::Instant::now();

    let mut input = None;
    let mut output = None;
    let mut map = None;
    let mut es6_syntax = false;
    let mut es6_syntax_everywhere = false;
    let mut map_inline = false;
    let mut no_map = false;
    let mut watch = false;
    let mut quiet_watch = false;

    let mut iter = env::args().skip(1);
    while let Some(arg) = iter.next() {
        for arg in expand_arg(arg) {
            match &*arg {
                "-h" | "--help" => return Err(CliError::Help),
                "-v" | "--version" => return Err(CliError::Version),
                "-w" | "--watch" => watch = true,
                "-W" | "--quiet-watch" => {
                    watch = true;
                    quiet_watch = true;
                },
                "-I" | "--map-inline" => map_inline = true,
                "-M" | "--no-map" => no_map = true,
                "-e" | "--es-syntax" => es6_syntax = true,
                "-E" | "--es-syntax-everywhere" => {
                    es6_syntax = true;
                    es6_syntax_everywhere = true;
                }
                "-m" | "--map" => {
                    if map.is_some() {
                        return Err(CliError::DuplicateOption(arg))
                    }
                    map = Some(iter.next().ok_or_else(|| CliError::MissingOptionValue(arg))?)
                }
                "-i" | "--input" => {
                    if input.is_some() {
                        return Err(CliError::DuplicateOption(arg))
                    }
                    input = Some(iter.next().ok_or_else(|| CliError::MissingOptionValue(arg))?)
                }
                "-o" | "--output" => {
                    if output.is_some() {
                        return Err(CliError::DuplicateOption(arg))
                    }
                    output = Some(iter.next().ok_or_else(|| CliError::MissingOptionValue(arg))?)
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
    }

    if map_inline as u8 + no_map as u8 + map.is_some() as u8 > 1 {
        return Err(CliError::BadUsage("--map-inline, --map <file>, and --no-map are mutually exclusive"))
    }

    let input = input.ok_or(CliError::MissingFileName)?;
    let input_dir = env::current_dir()?;
    let output = output.unwrap_or_else(|| "-".to_owned());

    let map_output = if map_inline {
        SourceMapOutput::Inline
    } else if no_map {
        SourceMapOutput::Suppressed
    } else {
        match map {
            Some(path) => {
                SourceMapOutput::File(PathBuf::from(path), Path::new(&output))
            }
            None => {
                if output == "-" {
                    SourceMapOutput::Suppressed
                } else {
                    let mut buf = OsString::from(&output);
                    buf.push(".map");
                    SourceMapOutput::File(PathBuf::from(buf), Path::new(&output))
                }
            }
        }
    };

    let input_options = InputOptions {
        es6_syntax,
        es6_syntax_everywhere,
    };

    let entry_point = Worker::resolve_main(input_options, input_dir, &input)?;

    if watch {
        let mut modules = bundle(&entry_point, input_options, &output, &map_output)?;
        let elapsed = entry_inst.elapsed();
        let ms = elapsed.as_secs() * 1_000 + (elapsed.subsec_nanos() / 1_000_000) as u64;

        let (tx, rx) = mpsc::channel();
        let debounce_dur = time::Duration::from_millis(5);
        let mut watcher = notify::raw_watcher(tx.clone())?;

        for path in modules.keys() {
            watcher.watch(path, notify::RecursiveMode::NonRecursive)?;
        }

        eprintln!("ready in {} ms", ms);

        loop {
            let first_event = rx.recv().expect("notify::watcher disconnected");
            thread::sleep(debounce_dur);
            for event in iter::once(first_event).chain(rx.try_iter()) {
                let _op = event.op?;
            }

            let start_inst = time::Instant::now();
            match bundle(&entry_point, input_options, &output, &map_output) {
                Ok(new_modules) => {
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
                Err(kind) => {
                    eprintln!("{}error: {}", if quiet_watch { "" } else { "\x07" }, kind);
                }
            }
        }
    } else {
        bundle(&entry_point, input_options, &output, &map_output).map(|_| ())
    }
}

const APP_NAME: &str = env!("CARGO_PKG_NAME");
const APP_VERSION: &str = env!("CARGO_PKG_VERSION");

fn write_usage(f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "\
Usage: {0} [options] <input> [output]
       {0} [-h | --help | -v | --version]", APP_NAME)
}

fn write_version(f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{0} v{1}", APP_NAME, APP_VERSION)
}

fn write_help(f: &mut fmt::Formatter) -> fmt::Result {
    write_version(f)?;
    write!(f, "\n\n")?;
    write_usage(f)?;
    write!(f, "\n\n")?;
    write!(f, "\
Options:
    -i, --input <input>
        Use <input> as the main module.

    -o, --output <output>
        Write bundle to <output> and source map to <output>.map.
        Default: '-' for stdout.

    -m, --map <map>
        Output source map to <map>.

    -I, --map-inline
        Output source map inline as data: URI.

    -M, --no-map
        Suppress source map output when it would normally be implied.

    -w, --watch
        Watch for changes to <input> and its dependencies.

    -W, --quiet-watch
        Don't emit a bell character for errors that occur while watching.
        Implies --watch.

    -e, --es-syntax
        Support .mjs files with ECMAScript module syntax:

            import itt from 'itt'
            export const greeting = 'Hello, world!'

        Instead of CommonJS require syntax:

            const itt = require('itt')
            exports.greeting = 'Hello, world!'

        .mjs (ESM) files can import .js (CJS) files, in which case the
        namespace object has a single `default` binding which reflects the
        value of `module.exports`. CJS files can require ESM files, in which
        case the resultant object is the namespace object.

    -E, --es-syntax-everywhere
        Implies --es-syntax. Allow ECMAScript module syntax in .js files.
        CJS-style `require()` calls are also allowed.

    -h, --help
        Print this message.

    -v, --version
        Print version information.
")
}

#[derive(Debug)]
pub enum CliError {
    Help,
    Version,
    MissingFileName,
    DuplicateOption(String),
    MissingOptionValue(String),
    UnknownOption(String),
    UnexpectedArg(String),
    BadUsage(&'static str),

    RequireRoot { context: Option<PathBuf>, path: PathBuf },
    EmptyModuleName { context: PathBuf },
    ModuleNotFound { context: PathBuf, name: String },
    MainNotFound { name: String },

    Io(io::Error),
    Json(json::Error),
    Notify(notify::Error),
    Es6(es6::Error),
    Lex(lex::Error),
    ParseStrLit(lex::ParseStrLitError),
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
impl From<es6::Error> for CliError {
    fn from(inner: es6::Error) -> CliError {
        CliError::Es6(inner)
    }
}
impl From<lex::Error> for CliError {
    fn from(inner: lex::Error) -> CliError {
        CliError::Lex(inner)
    }
}
impl From<lex::ParseStrLitError> for CliError {
    fn from(inner: lex::ParseStrLitError) -> CliError {
        CliError::ParseStrLit(inner)
    }
}
impl From<Box<Any + Send + 'static>> for CliError {
    fn from(inner: Box<Any + Send + 'static>) -> CliError {
        CliError::Box(inner)
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CliError::Help => {
                write_help(f)
            }
            CliError::Version => {
                write_version(f)
            }
            CliError::MissingFileName => {
                write_usage(f)
            }
            CliError::DuplicateOption(ref opt) => {
                write!(f, "option {} specified more than once", opt)
            }
            CliError::MissingOptionValue(ref opt) => {
                write!(f, "missing value for option {}", opt)
            }
            CliError::UnknownOption(ref opt) => {
                write!(f, "unknown option {}", opt)
            }
            CliError::UnexpectedArg(ref arg) => {
                write!(f, "unexpected argument {}", arg)
            }
            CliError::BadUsage(ref arg) => {
                write!(f, "{}", arg)
            }

            CliError::RequireRoot { ref context, ref path } => {
                match *context {
                    None => {
                        write!(f,
                            "main module is root path {}",
                            path.display(),
                        )
                    }
                    Some(ref context) => {
                        write!(f,
                            "require of root path {} in {}",
                            path.display(),
                            context.display(),
                        )
                    }
                }
            }
            CliError::EmptyModuleName { ref context } => {
                write!(f, "require('') in {}", context.display())
            }
            CliError::ModuleNotFound { ref context, ref name } => {
                write!(f,
                    "module '{}' not found in {}",
                    name,
                    context.display(),
                )
            }
            CliError::MainNotFound { ref name } => {
                write!(f, "main module '{}' not found", name)
            }

            CliError::Io(ref inner) => {
                write!(f, "{}", inner)
            }
            CliError::Json(ref inner) => {
                write!(f, "{}", inner)
            }
            CliError::Notify(ref inner) => {
                write!(f, "{}", inner)
            }
            CliError::Es6(ref inner) => {
                write!(f, "{}", inner)
            }
            CliError::Lex(ref inner) => {
                write!(f, "{}", inner)
            }
            CliError::ParseStrLit(ref inner) => {
                write!(f, "{}", inner)
            }
            CliError::Box(ref inner) => {
                write!(f, "{:?}", inner)
            }
        }
    }
}

fn main() {
    process::exit(match run() {
        Ok(_) => 0,
        Err(kind) => {
            match kind {
                CliError::Help |
                CliError::Version |
                CliError::MissingFileName => {
                    println!("{}", kind);
                }
                _ => {
                    println!("{}: {}", APP_NAME, kind);
                }
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
            if let Err(_) = self.tx.send(match work {
                Work::Resolve { context, name } => {
                    self.resolve(&context, &name)
                    .map(|resolved| WorkDone::Resolve {
                        context,
                        name,
                        resolved,
                    })
                }
                Work::Include { module } => {
                    self.include(&module)
                    .map(|info| WorkDone::Include {
                        module,
                        info,
                    })
                }
            }) { return }
        }
    }

    fn resolve_main(input_options: InputOptions, mut dir: PathBuf, name: &str) -> Result<PathBuf, CliError> {
        dir.append_resolving(Path::new(name));
        Self::resolve_path_or_module(input_options, None, dir)?.ok_or_else(|| {
            CliError::MainNotFound {
                name: name.to_owned(),
            }
        })
    }

    fn resolve(&self, context: &Path, name: &str) -> Result<Resolved, CliError> {
        // match name.chars().next() {
        match name.as_bytes().get(0) {
            None => Err(CliError::EmptyModuleName {
                context: context.to_owned()
            }),
            // TODO absolute paths on windows?
            Some(&b'.') => {
                let mut dir = context.to_owned();
                let did_pop = dir.pop(); // to directory
                debug_assert!(did_pop);
                dir.append_resolving(Path::new(name));
                Ok(Resolved::Normal(
                    Self::resolve_path_or_module(self.input_options, Some(context), dir)?.ok_or_else(|| {
                        CliError::ModuleNotFound {
                            context: context.to_owned(),
                            name: name.to_owned(),
                        }
                    })?,
                ))
            }
            Some(&b'/') => {
                Ok(Resolved::Normal(
                    Self::resolve_path_or_module(self.input_options, Some(context), PathBuf::from(name))?.ok_or_else(|| {
                        CliError::ModuleNotFound {
                            context: context.to_owned(),
                            name: name.to_owned(),
                        }
                    })?,
                ))
            }
            _ => {
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
                    if let Some(result) = Self::resolve_path_or_module(self.input_options, Some(context), new_path)? {
                        return Ok(Resolved::Normal(result))
                    }
                }

                Err(CliError::ModuleNotFound {
                    context: context.to_owned(),
                    name: name.to_owned(),
                })
            }
        }
    }
    fn resolve_path_or_module(input_options: InputOptions, context: Option<&Path>, mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
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
        Self::resolve_path(input_options, context, path)
    }

    fn resolve_path(input_options: InputOptions, context: Option<&Path>, mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
        // <path>
        if path.is_file() {
            return Ok(Some(path))
        }

        let file_name = path.file_name().ok_or_else(|| CliError::RequireRoot {
            context: context.map(|p| p.to_owned()),
            path: path.clone(),
        })?.to_owned();

        if input_options.es6_syntax {
            // <path>.mjs
            let mut mjs_file_name = file_name.to_owned();
            mjs_file_name.push(".mjs");
            path.set_file_name(&mjs_file_name);
            if path.is_file() {
                return Ok(Some(path))
            }

            // <path>/index.mjs
            path.set_file_name(&file_name);
            path.push("index.mjs");
            if path.is_file() {
                return Ok(Some(path))
            }
            path.pop();
        }

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

    fn include(&self, module: &Path) -> Result<ModuleInfo, CliError> {
        let mut source = String::new();
        let file = fs::File::open(module)?;
        let mut buf_reader = io::BufReader::new(file);
        buf_reader.read_to_string(&mut source)?;
        let mut new_source = None;
        let prefix;
        let suffix;

        let deps = {
            let path_string = module.to_string_lossy();
            // module.to_str().ok_or("<path with invalid utf-8>")
            let mut lexer = lex::Lexer::new(path_string.as_ref(), &source);

            let deps;
            let ext = module.extension();
            if matches!(ext, Some(s) if s == "mjs") {
                let module = es6::module_to_cjs(&mut lexer, false)?;
                // println!("{:#?}", module);
                deps = module.deps;
                prefix = module.source_prefix;
                suffix = module.source_suffix;
                new_source = Some(module.source);

            } else if matches!(ext, Some(s) if s == "json") {
                deps = HashSet::new();
                prefix = "module.exports =".to_owned();
                suffix = String::new();

            } else if self.input_options.es6_syntax_everywhere {
                let module = es6::module_to_cjs(&mut lexer, true)?;
                // println!("{:#?}", module);
                deps = module.deps;
                prefix = module.source_prefix;
                suffix = module.source_suffix;
                new_source = Some(module.source);

            } else {
                deps = cjs_parse_deps(&mut lexer)?;
                prefix = String::new();
                suffix = String::new();
            }

            if let Some(error) = lexer.take_error() {
                return Err(From::from(error))
            }

            deps.into_iter()
                .map(|s| s.into_owned())
                .collect()
        };

        Ok(ModuleInfo {
            source: match new_source {
                None => Source {
                    prefix,
                    body: source,
                    suffix,
                    original: None,
                },
                Some(new_source) => Source {
                    prefix,
                    body: new_source,
                    suffix,
                    original: Some(source),
                }
            },
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

#[cfg(test)]
mod test;
