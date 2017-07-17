use std::{env, process};

fn print_usage() {
    println!("usage: esparse <file>");
}

fn run() -> Result<(), ()> {
    let file_name = match env::args().nth(1) {
        Some(f) => f,
        None => {
            print_usage();
            return Err(())
        },
    };
    unimplemented!();
}

fn main() {
    process::exit(match run() {
        Ok(_) => 0,
        Err(_) => 1,
    })
}
