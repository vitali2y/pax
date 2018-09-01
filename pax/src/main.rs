#![cfg_attr(all(test, feature = "bench"), feature(test))]

#[macro_use]
extern crate esparse;
extern crate crossbeam;
extern crate num_cpus;
extern crate notify;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate memchr;
extern crate base64;
extern crate regex;
extern crate fnv;
#[macro_use]
extern crate matches;
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use]
extern crate cfg_if;

use std::{env, process, io, fs, thread, time, iter, fmt, str, string};
use std::io::prelude::*;
use std::fmt::{Display, Write};
use std::path::{self, PathBuf, Path, Component};
use std::sync::mpsc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::any::Any;
use std::borrow::Cow;
use std::ffi::OsString;
use fnv::{FnvHashMap, FnvHashSet};
use crossbeam::sync::SegQueue;
use notify::Watcher;
use esparse::lex::{self, Tt};
use serde::ser::{Serialize, Serializer, SerializeSeq};
use serde_json::Value;
use regex::Regex;

mod opts;
mod es6;

const HEAD_JS: &str = include_str!("head.js");
const TAIL_JS: &str = include_str!("tail.js");
const CORE_MODULES: &[&str] = &["assert", "buffer", "child_process", "cluster", "crypto", "dgram", "dns", "domain", "events", "fs", "http", "https", "net", "os", "path", "punycode", "querystring", "readline", "stream", "string_decoder", "tls", "tty", "url", "util", "v8", "vm", "zlib"];

fn cjs_parse_deps<'f, 's>(lex: &mut lex::Lexer<'f, 's>) -> Result<FnvHashSet<Cow<'s, str>>, CliError> {
    // TODO should we panic on dynamic requires?
    let mut deps = FnvHashSet::default();
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
    modules: FnvHashMap<PathBuf, Module>,
    entry_point: &'a Path,
    map_output: &'b SourceMapOutput<'b>,
}

impl<'a, 'b> Writer<'a, 'b> {
    fn sorted_modules(&self) -> Vec<(&Path, &Module)> {
        let mut modules = self.modules
            .iter()
            .map(|(p, m)| (p.as_path(), m))
            .collect::<Vec<_>>();
        modules.sort_by(|(f, _), (g, _)| f.cmp(g));
        modules
    }

    fn write_to<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        w.write_all(HEAD_JS.as_bytes())?;
        // for (module, main) in self.mains {
        //     write!(w,
        //         "\n  Pax.mains[{mod_path}] = {main_path}",
        //         mod_path = Self::js_path(&module),
        //         main_path = Self::js_path(&main),
        //     );
        // }

        for (file, info) in self.sorted_modules() {
            let id = Self::name_path(&file);
            let deps = Self::stringify_deps(&info.deps);
            let filename = Self::js_path(&file);

            write!(w,
                "\n  Pax.files[{filename}] = {id}; {id}.deps = {deps}; {id}.filename = {filename}; function {id}(module, exports, require, __filename, __dirname, __import_meta) {{\n",
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
            "\n  Pax.main = {main}; Pax.makeRequire(null)()\n  if (typeof module !== 'undefined') module.exports = Pax.main.module && Pax.main.module.exports\n",
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

    fn write_map_to<W: io::Write>(&self, w: &mut W) -> serde_json::Result<()> {
        // https://sourcemaps.info/spec.html

        let ref modules = self.sorted_modules();
        let dir = self.entry_point.parent().unwrap();

        #[derive(Serialize, Debug)]
        #[serde(rename_all = "camelCase")]
        struct SourceMap<'a> {
            version: u8,
            file: &'static str,
            source_root: &'static str,
            sources: Sources<'a>,
            sources_content: SourcesContent<'a>,
            names: [(); 0],
            mappings: Mappings<'a>,
        }

        #[derive(Debug)]
        struct Sources<'a> {
            modules: &'a [(&'a Path, &'a Module)],
            dir: &'a Path,
        }

        impl<'a> Serialize for Sources<'a> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let mut seq = serializer.serialize_seq(None)?;
                for (f, _) in self.modules {
                    let relative = f.relative_from(self.dir);
                    let path = relative.as_ref().map_or(*f, PathBuf::as_path);
                    seq.serialize_element(&path.to_string_lossy())?;
                }
                seq.end()
            }
        }

        #[derive(Debug)]
        struct SourcesContent<'a> {
            modules: &'a [(&'a Path, &'a Module)],
        }

        impl<'a> Serialize for SourcesContent<'a> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let mut seq = serializer.serialize_seq(None)?;
                for (_, module) in self.modules {
                    let content = module.source.original.as_ref().unwrap_or(&module.source.body);
                    seq.serialize_element(content)?;
                }
                seq.end()
            }
        }

        #[derive(Debug)]
        struct Mappings<'a> {
            modules: &'a [(&'a Path, &'a Module)],
        }

        impl<'a> Serialize for Mappings<'a> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                serializer.collect_str(self)
            }
        }

        impl<'a> Display for Mappings<'a> {
            fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
                let prefix_len = count_lines(HEAD_JS); /*+ this.mains.size*/
                for _ in 0..prefix_len {
                    w.write_str(";")?;
                }

                let mut line = 0;
                let mut vlq = Vlq::new();
                for (index, &(_, module)) in self.modules.iter().enumerate() {
                    w.write_str(";")?;
                    if !module.source.prefix.is_empty() {
                        for _ in 0..count_lines(&module.source.prefix) {
                            w.write_str(";")?;
                        }
                    }
                    for i in 0..count_lines(&module.source.body) {
                        w.write_str("A")?;
                        if i == 0 {
                            if index == 0 {
                                w.write_str("AAA")?;
                            } else {
                                w.write_str("C")?;
                                w.write_str(vlq.enc(-line))?;
                                w.write_str("A")?;
                            }
                            line = 0;
                        } else {
                            w.write_str("ACA")?;
                            line += 1;
                        }
                        w.write_str(";")?;
                    }
                    if !matches!(module.source.body.chars().last(), None | Some('\n') | Some('\r') | Some('\u{2028}') | Some('\u{2029}')) {
                        w.write_str(";")?;
                    }
                    for _ in 0..count_lines(&module.source.suffix)-1 {
                        w.write_str(";")?;
                    }
                }
                for _ in 0..2 + count_lines(TAIL_JS) + 1 - 1 - 1 {
                    w.write_str(";")?;
                }
                Ok(())
            }
        }

        serde_json::to_writer(w, &SourceMap {
            version: 3,
            file: "",
            source_root: "",
            sources: Sources { modules, dir },
            sources_content: SourcesContent { modules },
            names: [],
            mappings: Mappings { modules },
        })
    }

    fn stringify_deps(deps: &FnvHashMap<String, Resolved>) -> String {
        let mut result = "{".to_owned();
        let mut comma = false;
        for (name, resolved) in deps {
            match *resolved {
                Resolved::External => {}
                Resolved::Normal(ref path) => {
                    if comma {
                        result.push(',');
                    }
                    result.push_str(&to_quoted_json_string(name));
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
        let replaced = string.replace('\\', "/");
        to_quoted_json_string(&replaced)
    }

    #[cfg(not(target_os = "windows"))]
    fn js_path(path: &Path) -> String {
        let string = path.to_string_lossy();
        to_quoted_json_string(&string)
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

fn to_quoted_json_string(s: &str) -> String {
    // Serializing to a String only fails if the Serialize impl decides to fail, which the Serialize impl of `str` never does.
    serde_json::to_string(s).unwrap()
}

fn count_lines(source: &str) -> usize {
    // TODO non-ASCII line terminators?
    1 + memchr::Memchr::new(b'\n', source.as_bytes()).count()
}

struct Vlq {
    buf: [u8; 13],
}
impl Vlq {
    fn new() -> Self {
        Self {
            buf: [0u8; 13],
        }
    }

    fn enc(&mut self, n: isize) -> &str {
        let sign = n < 0;
        let n = if sign { -n } else { n } as usize;
        let mut y = (n & 0xf) << 1 | sign as usize;
        let mut r = n >> 4;
        let mut l = 0;
        while r > 0 {
            y |= 0x20;
            self.buf[l] = B64[y];
            y = r & 0x1f;
            r >>= 5;
            l += 1;
        }
        self.buf[l] = B64[y];
        str::from_utf8(&self.buf[0..l+1]).unwrap()
    }
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
    pub deps: FnvHashMap<String, Resolved>,
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
    External,
    // CoreWithSubst(PathBuf),
    Normal(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct InputOptions {
    pub es6_syntax: bool,
    pub es6_syntax_everywhere: bool,
    pub external: FnvHashSet<String>,
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

pub fn bundle(entry_point: &Path, input_options: InputOptions, output: &str, map_output: &SourceMapOutput) -> Result<FnvHashMap<PathBuf, Module>, CliError> {
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

    let mut modules = FnvHashMap::<PathBuf, ModuleState>::default();

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
                    Resolved::External => {}
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
                    deps: FnvHashMap::default(),
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
    let mut external = FnvHashSet::default();

    let mut iter = opts::args();
    while let Some(arg) = iter.next() {
        let opt = match arg {
            opts::Arg::Pos(arg) => {
                if input.is_none() {
                    input = Some(arg)
                } else if output.is_none() {
                    output = Some(arg)
                } else {
                    return Err(CliError::UnexpectedArg(arg))
                }
                continue
            }
            opts::Arg::Opt(opt) => opt,
        };
        match &*opt {
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
            "-x" | "--external" => {
                lazy_static! {
                    static ref COMMA: Regex = Regex::new(r#"\s*,\s*"#).unwrap();
                }
                let mods = iter.next_arg().ok_or_else(|| CliError::MissingOptionValue(opt))?;
                for m in COMMA.split(&mods) {
                    external.insert(m.to_string());
                }
            }
            "--external-core" => {
                for m in CORE_MODULES {
                    external.insert(m.to_string());
                }
            }
            "-m" | "--map" => {
                if map.is_some() {
                    return Err(CliError::DuplicateOption(opt))
                }
                map = Some(iter.next_arg().ok_or_else(|| CliError::MissingOptionValue(opt))?)
            }
            "-i" | "--input" => {
                if input.is_some() {
                    return Err(CliError::DuplicateOption(opt))
                }
                input = Some(iter.next_arg().ok_or_else(|| CliError::MissingOptionValue(opt))?)
            }
            "-o" | "--output" => {
                if output.is_some() {
                    return Err(CliError::DuplicateOption(opt))
                }
                output = Some(iter.next_arg().ok_or_else(|| CliError::MissingOptionValue(opt))?)
            }
            _ => {
                return Err(CliError::UnknownOption(opt))
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
        external,
    };

    let entry_point = Worker::resolve_main(&input_options, input_dir, &input)?;

    if watch {
        let progress_line = format!(" build {output} ...", output = output);
        eprint!("{}", progress_line);
        io::Write::flush(&mut io::stderr())?;

        let mut modules = bundle(&entry_point, input_options.clone(), &output, &map_output)?;
        let elapsed = entry_inst.elapsed();
        let ms = elapsed.as_secs() * 1_000 + u64::from(elapsed.subsec_millis());

        let (tx, rx) = mpsc::channel();
        let debounce_dur = time::Duration::from_millis(5);
        let mut watcher = notify::raw_watcher(tx.clone())?;

        for path in modules.keys() {
            watcher.watch(path, notify::RecursiveMode::NonRecursive)?;
        }

        eprintln!("{bs} ready {output} in {ms} ms", output = output, ms = ms, bs = "\u{8}".repeat(progress_line.len()));

        loop {
            let first_event = rx.recv().expect("notify::watcher disconnected");
            thread::sleep(debounce_dur);
            for event in iter::once(first_event).chain(rx.try_iter()) {
                let _op = event.op?;
            }

            eprint!("update {} ...", output);
            io::Write::flush(&mut io::stderr())?;
            let start_inst = time::Instant::now();
            match bundle(&entry_point, input_options.clone(), &output, &map_output) {
                Ok(new_modules) => {
                    let elapsed = start_inst.elapsed();
                    let ms = elapsed.as_secs() * 1_000 + u64::from(elapsed.subsec_millis());
                    eprintln!("{bs}in {ms} ms", ms = ms, bs = "\u{8}".repeat(3));

                    {
                        let mut to_unwatch = modules.keys().collect::<FnvHashSet<_>>();
                        let mut to_watch = new_modules.keys().collect::<FnvHashSet<_>>();
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
const EXE_NAME: &str = "px";
const APP_VERSION: &str = env!("CARGO_PKG_VERSION");

fn write_usage(f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "\
Usage: {0} [options] <input> [output]
       {0} [-h | --help | -v | --version]", EXE_NAME)
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

    -x, --external <module1,module2,...>
        Don't resolve or include modules named <module1>, <module2>, etc.;
        leave them as require('<module>') references in the bundle. Specifying
        a path instead of a module name does nothing.

    --external-core
        Ignore references to node.js core modules like 'events' and leave them
        as require('<module>') references in the bundle.

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

    InvalidUtf8 { context: PathBuf, err: string::FromUtf8Error },

    Io(io::Error),
    Json(serde_json::Error),
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
impl From<serde_json::Error> for CliError {
    fn from(inner: serde_json::Error) -> CliError {
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

            CliError::InvalidUtf8 { ref context, ref err } => {
                write!(f, "in {}: {}", context.display(), err)
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
                    println!("{}: {}", EXE_NAME, kind);
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
                }
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
            let work_done = match work {
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
            };
            if self.tx.send(work_done).is_err() { return }
        }
    }

    fn resolve_main(input_options: &InputOptions, mut dir: PathBuf, name: &str) -> Result<PathBuf, CliError> {
        dir.append_resolving(Path::new(name));
        Self::resolve_path_or_module(input_options, None, dir)?.ok_or_else(|| {
            CliError::MainNotFound {
                name: name.to_owned(),
            }
        })
    }

    fn resolve(&self, context: &Path, name: &str) -> Result<Resolved, CliError> {
        if name.is_empty() {
            return Err(CliError::EmptyModuleName {
                context: context.to_owned()
            })
        }
        let path = Path::new(name);
        if path.is_absolute() {
            Ok(Resolved::Normal(
                Self::resolve_path_or_module(&self.input_options, Some(context),path.to_owned())?.ok_or_else(|| {
                    CliError::ModuleNotFound {
                        context: context.to_owned(),
                        name: name.to_owned(),
                    }
                })?,
            ))
        } else if path.starts_with(".") || path.starts_with("..") {
            let mut dir = context.to_owned();
            let did_pop = dir.pop(); // to directory
            debug_assert!(did_pop);
            dir.append_resolving(path);
            Ok(Resolved::Normal(
                Self::resolve_path_or_module(&self.input_options, Some(context), dir)?.ok_or_else(|| {
                    CliError::ModuleNotFound {
                        context: context.to_owned(),
                        name: name.to_owned(),
                    }
                })?,
            ))
        } else {
            if self.input_options.external.contains(name) {
                return Ok(Resolved::External)
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
                if let Some(result) = Self::resolve_path_or_module(&self.input_options, Some(context), new_path)? {
                    return Ok(Resolved::Normal(result))
                }
            }

            Err(CliError::ModuleNotFound {
                context: context.to_owned(),
                name: name.to_owned(),
            })
        }
    }
    fn resolve_path_or_module(input_options: &InputOptions, context: Option<&Path>, mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
        path.push("package.json");
        let result = fs::File::open(&path);
        path.pop();
        match result {
            Ok(file) => {
                let string = {
                    let mut buf_reader = io::BufReader::new(file);
                    let mut bytes = Vec::new();
                    buf_reader.read_to_end(&mut bytes)?;
                    match String::from_utf8(bytes) {
                        Ok(s) => s,
                        Err(err) => {
                            return Err(CliError::InvalidUtf8 {
                                context: path.join("package.json"),
                                err,
                            })
                        }
                    }
                };

                #[derive(Deserialize)]
                struct Package {
                    #[serde(default)]
                    main: Value,

                    // Efficiently skip deserializing other fields.
                }

                let result: Package = serde_json::from_str(&string)?;
                match result.main.as_str() {
                    None => {
                        // Do nothing if `main` is not present or not a string.
                    }
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

    fn resolve_path(input_options: &InputOptions, context: Option<&Path>, mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
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
        let source = {
            let file = fs::File::open(module)?;
            let mut buf_reader = io::BufReader::new(file);
            let mut bytes = Vec::new();
            buf_reader.read_to_end(&mut bytes)?;
            match String::from_utf8(bytes) {
                Ok(s) => s,
                Err(err) => return Err(CliError::InvalidUtf8 {
                    context: module.to_owned(),
                    err,
                }),
            }
        };
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
                deps = FnvHashSet::default();
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
