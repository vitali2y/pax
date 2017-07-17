use std::{env, process};

fn run() -> Result<(), CliError> {
    let file_name = match env::args().nth(1) {
        Some(f) => f,
        None => {
            print_usage();
            return Err(CliError::MissingFileName)
        }
    };
    unimplemented!();
}

fn print_usage() {
    println!("usage: esparse <file>");
}

enum CliError {
    MissingFileName,
}

fn main() {
    process::exit(match run() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
