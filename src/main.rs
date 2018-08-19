extern crate yarlox;

use {yarlox::run_file, yarlox::run_prompt, std::env};

fn main() {
    let in_file = env::args().skip(1).next();
    match in_file {
        Some(file) => run_file(&file).unwrap(),
        None => run_prompt().unwrap(),
    };
}
