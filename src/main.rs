use std::{env, process, io, fs};
use std::io::prelude::*;

fn run() -> Result<(), CliError> {
    let file_name = env::args().nth(1).ok_or(CliError::MissingFileName)?;
    let file = fs::File::open(file_name)?;
    let mut buf_reader = io::BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    unimplemented!();
}

fn print_usage() {
    println!("usage: esparse <file>");
}

enum CliError {
    MissingFileName,
    Io(io::Error),
}
impl From<io::Error> for CliError {
    fn from(inner: io::Error) -> CliError {
        CliError::Io(inner)
    }
}

fn main() {
    process::exit(match run() {
        Ok(_) => 0,
        Err(kind) => {
            match kind {
                CliError::MissingFileName => print_usage(),
                CliError::Io(inner) => println!("{}", inner),
            }
            1
        }
    })
}
