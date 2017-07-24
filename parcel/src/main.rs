extern crate esparse;
extern crate crossbeam;
extern crate num_cpus;
extern crate json;
extern crate notify;
#[macro_use]
extern crate matches;

use std::{env, process, io, fs, thread};
use std::io::prelude::*;
use std::fmt::Write;
use std::path::{self, PathBuf, Path, Component};
use std::sync::mpsc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::collections::{HashMap, HashSet};
use std::any::Any;
use std::borrow::Cow;
use crossbeam::sync::SegQueue;

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
struct Writer {
    modules: HashMap<PathBuf, Module>,
    entry_point: PathBuf,
}

impl Writer {
    fn write_to<W: io::Write>(&self, w: &mut W) -> Result<(), io::Error> {
        w.write(HEAD_JS.as_bytes())?;
        // for (module, main) in self.mains {
        //     write!(w,
        //         "\n  Parcel.mains[{mod_path}] = {main_path}",
        //         mod_path = Self::js_path(&module),
        //         main_path = Self::js_path(&main),
        //     );
        // }
        let mut modules = self.modules.iter().collect::<Vec<_>>();
        modules.sort_by(|&(ref f, _), &(ref g, _)| f.cmp(g));

        for (file, info) in modules {
            let id = Self::name_path(&file);
            let prefix = if matches!(file.extension(), Some(s) if s == ".json") {
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
            w.write(info.source.as_bytes())?;
            write!(w, "}}")?;
        }
        let main = Self::name_path(&self.entry_point);
        write!(w,
            "\n  Parcel.main = {main}; Parcel.makeRequire(null)()\n  if (typeof module !== 'undefined') module.exports = Parcel.main.module && Parcel.main.module.exports",
            main = main,
        )?;
        w.write(TAIL_JS.as_bytes())?;
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
            let c = b as char;
            match c {
                '_' | 'a'...'z' | 'A'...'Z' => {
                    result.push(c);
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

#[derive(Debug, Clone)]
struct Worker {
    tx: mpsc::Sender<Result<WorkDone, CliError>>,
    queue: Arc<SegQueue<Work>>,
    quit: Arc<AtomicBool>,
}

// TODO use references
#[derive(Debug)]
enum Work {
    ResolveMain { dir: PathBuf, name: String },
    Resolve { context: PathBuf, name: String },
    Include { module: PathBuf },
}
#[derive(Debug)]
enum WorkDone {
    ResolveMain { resolved: PathBuf },
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

fn bundle(input: String, input_dir: PathBuf, output: &str) -> Result<(), CliError> {
    let thread_count = num_cpus::get();
    let (tx, rx) = mpsc::channel();
    let worker = Worker {
        tx,
        quit: Arc::new(AtomicBool::new(false)),
        queue: Arc::new(SegQueue::new()),
    };

    let mut pending = 0;
    worker.add_work(Work::ResolveMain { dir: input_dir, name: input });
    pending += 1;

    let children: Vec<_> = (0..thread_count).map(|_| {
        let worker = worker.clone();
        thread::spawn(move || worker.run())
    }).collect();

    let mut modules = HashMap::<PathBuf, ModuleState>::new();
    let mut entry_point = None;

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
            WorkDone::ResolveMain { resolved } => {
                debug_assert_matches!(entry_point, None);
                entry_point = Some(resolved.clone());
                modules.entry(resolved.clone()).or_insert_with(|| {
                    worker.add_work(Work::Include { module: resolved });
                    pending += 1;
                    ModuleState::Loading
                });
            }
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
        entry_point: entry_point.unwrap(),
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
    // println!("entry point: {:?}", entry_point);
    // println!("{:#?}", modules);

    Ok(())
}

fn run() -> Result<(), CliError> {
    let mut input = None;
    let mut output = None;
    let mut watch = false;

    for arg in env::args().skip(1) {
        match arg.as_ref() {
            "-h" | "--help" => return Err(CliError::Help),
            "-w" | "--watch" => watch = true,
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

    let input = input.ok_or(CliError::MissingFileName)?;
    let input_dir = env::current_dir()?;
    let output = output.unwrap_or("-".to_owned());

    // println!("{} => {} (watching: {:?})", input, output, watch);
    // println!();

    bundle(input, input_dir, &output)
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

arguments:
    <input>        input filename
    [output]       output filename or '-' for stdout (default '-')

options:
    -w, --watch    watch for changes to <input> and its dependencies
    -h, --help     print this message
", APP_NAME, APP_VERSION);
}

#[derive(Debug)]
enum CliError {
    Help,
    MissingFileName,
    UnknownOption(String),
    UnexpectedArg(String),

    RequireRoot { path: PathBuf },
    EmptyModuleName { context: PathBuf },
    ModuleNotFound { context: PathBuf, name: String },

    Io(io::Error),
    Json(json::Error),
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
                CliError::UnknownOption(opt) => println!("{}: unknown option {}", APP_NAME, opt),
                CliError::UnexpectedArg(arg) => println!("{}: unexpected argument {}", APP_NAME, arg),

                CliError::RequireRoot { path } => println!("{}: require of root path {}", APP_NAME, path.display()), // TODO in what module
                CliError::EmptyModuleName { context } => println!("{}: require('') in {}", APP_NAME, context.display()),
                CliError::ModuleNotFound { context, name } => println!("{}: module '{}' not found in {}", APP_NAME, name, context.display()),

                CliError::Io(inner) => println!("{}: {}", APP_NAME, inner),
                CliError::Json(inner) => println!("{}: {}", APP_NAME, inner),
                CliError::Box(inner) => println!("{}: {:?}", APP_NAME, inner),
            }
            1
        }
    })
}

trait AppendResolving {
    fn append_resolving<P: AsRef<Path> + ?Sized>(&mut self, more: &P);
}
impl AppendResolving for PathBuf {
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

impl Worker {
    fn run(mut self) {
        while let Some(work) = self.get_work() {
            self.tx.send(match work {
                Work::ResolveMain { dir, name } => {
                    self.resolve(&dir, &name, true)
                    .map(|resolved| match resolved {
                        Resolved::Normal(resolved) => {
                            WorkDone::ResolveMain {
                                resolved,
                            }
                        }
                        _ => panic!("non-normal entry point module"),
                    })
                }
                Work::Resolve { context, name } => {
                    self.resolve(&context, &name, false)
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
            }).unwrap();
        }
    }

    fn resolve(&self, context: &Path, name: &str, is_main: bool) -> Result<Resolved, CliError> {
        // match name.chars().next() {
        match name.as_bytes().get(0).map(|&b| b as char) {
            None => Err(CliError::EmptyModuleName {
                context: context.to_owned()
            }),
            Some('/') => {
                Ok(Resolved::Normal(
                    self.resolve_path_or_module(PathBuf::from(name))?.ok_or_else(|| {
                        CliError::ModuleNotFound {
                            context: context.to_owned(),
                            name: name.to_owned(),
                        }
                    })?,
                ))
            }
            Some('.') => {
                let mut dir = context.to_owned();
                if !is_main {
                    let did_pop = dir.pop(); // to directory
                    debug_assert!(did_pop);
                }
                dir.append_resolving(Path::new(name));
                Ok(Resolved::Normal(
                    self.resolve_path_or_module(dir)?.ok_or_else(|| {
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
                    match self.resolve_path_or_module(new_path)? {
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
    fn resolve_path_or_module(&self, mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
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
        self.resolve_path(path)
    }

    fn resolve_path(&self, mut path: PathBuf) -> Result<Option<PathBuf>, CliError> {
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

    fn include(&self, module: &Path) -> Result<ModuleInfo, CliError> {
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
