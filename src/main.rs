use std::env;

fn print_usage() -> ! {
    unimplemented!();
}

fn main() {
    let file_name = match env::args().nth(1) {
        Some(f) => f,
        None => print_usage(),
    };
    unimplemented!();
}
