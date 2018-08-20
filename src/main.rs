extern crate yarlox;

use {std::env, yarlox::run_file, yarlox::run_prompt};

fn main() {
    let in_file = env::args().skip(1).next();
    match in_file {
        Some(file) => run_file(&file).unwrap(),
        None => run_prompt().unwrap(),
    };
}
