use std::error::Error;
use std::io::{self, prelude::*};
use vm::{InterpretError, Vm};

#[macro_use]
extern crate log;
extern crate itertools;

pub mod chunk;
mod compiler;
mod object;
mod scanner;
pub mod value;
pub mod vm;

pub fn repl() -> Result<(), Box<dyn Error>> {
    let mut line = String::new();
    let mut vm = Vm::new();
    loop {
        line.clear();
        print!("\nlox> ");
        io::stdout().flush()?;

        let n = io::stdin().read_line(&mut line)?;
        if n == 0 {
            break;
        }
        if let Err(e) = interpret(&mut vm, &line) {
            eprintln!("{}", e);
        }
    }
    println!("");
    Ok(())
}

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let mut vm = Vm::new();
    let mut file_buf = String::new();
    std::fs::File::open(path)?.read_to_string(&mut file_buf)?;
    interpret(&mut vm, &file_buf)?;
    Ok(())
}

fn interpret(vm: &mut vm::Vm, s: &str) -> Result<(), InterpretError> {
    let chunk = compiler::compile(s).map_err(|_| InterpretError::CompileTime)?;
    vm.interpret(chunk)
}
