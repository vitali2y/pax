use std::{env, process};

fn print_usage() -> ! {
    println!("usage: esparse <file>");
    process::exit(1);
}

fn main() {
    let file_name = match env::args().nth(1) {
        Some(f) => f,
        None => print_usage(),
    };
    unimplemented!();
}
