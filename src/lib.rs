#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

mod ast_printer;
mod environment;
mod error;
mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;
mod token;
mod types;

use {
    error::{LoxError, LoxErrorTrait},
    interpreter::Interpreter,
    parser::Parser,
    scanner::Scanner,
    std::{
        fs, io,
        io::{BufRead, Read},
    },
};

pub fn run_file(file_name: &str) -> Result<(), io::Error> {
    let mut file = fs::File::open(file_name)?;
    let mut source = String::new();
    let mut interpreter = Interpreter::new();
    file.read_to_string(&mut source)?;
    match run(&mut interpreter, source) {
        Err(error) => error.report_exit(),
        Ok(ok) => Ok(ok),
    }
}

pub fn run_prompt() -> Result<(), io::Error> {
    let handle = io::stdin();
    let mut interpreter = Interpreter::new();
    loop {
        let mut source = String::new();
        match handle.lock().read_line(&mut source)? {
            0 => break,
            _ => match run(&mut interpreter, source) {
                Err(error) => error.report(),
                _ => (),
            },
        }
    }
    Ok(())
}

fn run(interpreter: &mut Interpreter, source: String) -> Result<(), LoxError> {
    let scanner = Scanner::new(&source);
    let stmts: Vec<stmt::Stmt> = Parser::new(scanner)?
        .parse()?
        .into_iter()
        .filter_map(|stmt| stmt)
        .collect();
    interpreter.interpret(&stmts)?;
    Ok(())
}
