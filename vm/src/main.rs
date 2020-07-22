extern crate env_logger;

use std::env;
use yarlox_vm::{repl, run_file};

fn main() {
    env_logger::init();
    let args: Vec<_> = env::args().collect();
    match args.len() {
        1 => repl().unwrap(),
        2 => run_file(&args[1]).unwrap(),
        _ => {
            eprintln!("Usage: yarlox [path]");
            std::process::exit(64);
        }
    }
}
