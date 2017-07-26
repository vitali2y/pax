extern crate esparse;

use std::{env, process, io, fs};
use std::io::prelude::*;
use esparse::lex;

fn run() -> Result<(), CliError> {
    let file_name = env::args().nth(1).ok_or(CliError::MissingFileName)?;
    let file = fs::File::open(&file_name)?;
    let mut buf_reader = io::BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let mut lexer = lex::Lexer::new(&file_name, &contents);
    loop {
        let tok = lexer.advance();
        write!(stdout, "{}{}", tok.ws_before, tok.tt).unwrap();
        if tok.tt == lex::Tt::Eof {
            break
        }
    }
    // writeln!(stdout).unwrap();
    Ok(())
}

const APP_NAME: &'static str = env!("CARGO_PKG_NAME");

fn print_usage() {
    println!("usage: {} <file>", APP_NAME);
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
                CliError::Io(inner) => println!("{}: {}", APP_NAME, inner),
            }
            1
        }
    })
}
