use std::error::Error;
use std::io::prelude::*;
use vm::{InterpretError, Vm};

#[macro_use]
extern crate log;
extern crate itertools;

pub mod chunk;
mod compiler;
mod scanner;
pub mod value;
pub mod vm;

pub fn repl() -> Result<(), Box<dyn Error>> {
    let mut line = String::new();
    loop {
        line.clear();
        let stdin = std::io::stdin();
        println!("lox> ");
        let n = stdin.read_line(&mut line)?;
        if n == 0 {
            break;
        }
        let _e = interpret(&line);
    }
    Ok(())
}

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let mut file_buf = String::new();
    std::fs::File::open(path)?.read_to_string(&mut file_buf)?;
    interpret(&file_buf)?;
    Ok(())
}

fn interpret(s: &str) -> Result<(), InterpretError> {
    let chunk = compiler::compile(s).map_err(|_| InterpretError::CompileTime)?;
    let mut vm = Vm::new(chunk);
    vm.interpret()
}
