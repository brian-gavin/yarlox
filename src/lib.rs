#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
extern crate rustyline;

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
    rustyline::{error::ReadlineError, Editor},
    scanner::Scanner,
    std::{
        fs,
        io::{self, prelude::*},
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
    let mut interpreter = Interpreter::new();
    let mut rl = Editor::<()>::new();
    if rl.load_history("./.loxhistory").is_err() {
        println!("No history file");
    }
    loop {
        let readline = rl.readline("lox> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match run(&mut interpreter, line) {
                    Err(e) => e.report(),
                    _ => (),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(e) => eprintln!("Readline error: {}", e),
        }
    }
    if rl.save_history("./.loxhistory").is_err() {
        println!("Error saving history file");
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
