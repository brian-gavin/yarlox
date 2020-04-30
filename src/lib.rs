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
mod resolver;
mod scanner;
mod stmt;
mod token;
mod types;

use {
    error::{LoxError, LoxErrorTrait},
    interpreter::Interpreter,
    parser::Parser,
    resolver::Resolver,
    rustyline::{error::ReadlineError, Editor},
    scanner::Scanner,
    std::{
        fs,
        io::{self, prelude::*},
    },
    stmt::Stmt,
};

pub fn run_file(file_name: &str) -> Result<(), io::Error> {
    let mut file = fs::File::open(file_name)?;
    let mut source = String::new();
    let mut interpreter = Interpreter::new();
    let mut stmts = Vec::new();
    file.read_to_string(&mut source)?;
    match run(&mut interpreter, &mut stmts, source, false) {
        Err(error) => error.report_exit(),
        Ok(ok) => Ok(ok),
    }
}

pub fn run_prompt() -> Result<(), io::Error> {
    let mut interpreter = Interpreter::new();
    let mut stmts = Vec::new(); // band-aid fix. better to have LoxType OWN it's stmt so that this doesn't have to be done
    let mut rl = Editor::<()>::new();
    if rl.load_history("./.loxhistory").is_err() {
        println!("No history file");
    }
    loop {
        let readline = rl.readline("lox> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match run(&mut interpreter, stmts, line, true) {
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

fn run<'a>(
    interpreter: &mut Interpreter<'a>,
    stmts: &'a mut Vec<Stmt>,
    source: String,
    repl: bool,
) -> Result<(), LoxError> {
    let old_len = stmts.len();
    let scanner = Scanner::new(&source);
    Parser::new(scanner, repl)?
        .parse()?
        .into_iter()
        .filter_map(|stmt| stmt)
        .for_each(|stmt| {
            stmts.push(stmt);
        });
    let new_stmts = &mut stmts[old_len..];
    Resolver::new().resolve(new_stmts)?;
    interpreter.interpret(new_stmts)?;
    Ok(())
}
