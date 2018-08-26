#[macro_use]
extern crate lazy_static;

mod ast_printer;
mod expr;
mod interpreter;
mod parser;
mod scanner;
mod token;
mod types;
mod visit;

use {
    scanner::Scanner,
    std::{
        error::Error,
        fmt, fs, io,
        io::{BufRead, Read},
        process,
    },
};

pub struct ParseError {
    line: u32,
    e_type: String,
    msg: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] Error {}: {}", self.line, self.e_type, self.msg)
    }
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Error for ParseError {}

impl ParseError {
    pub fn new(line: u32, e_type: String, msg: String) -> ParseError {
        ParseError {
            line: line,
            e_type: e_type,
            msg: msg,
        }
    }

    pub fn report(&self) {
        eprintln!("{}", self);
    }

    pub fn message(&self) -> String {
        format!("{}", self)
    }

    pub fn report_exit(self) -> ! {
        report_exit(self)
    }
}

pub fn report_exit(e: ParseError) -> ! {
    e.report();
    process::exit(0)
}

pub fn run_file(file_name: &str) -> Result<(), io::Error> {
    let mut file = fs::File::open(file_name)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    match run(source) {
        Err(error) => error.report_exit(),
        Ok(ok) => Ok(ok),
    }
}

pub fn run_prompt() -> Result<(), io::Error> {
    let handle = io::stdin();
    loop {
        let mut source = String::new();
        match handle.lock().read_line(&mut source)? {
            0 => break,
            _ => match run(source) {
                Err(error) => error.report(),
                _ => (),
            },
        }
    }
    Ok(())
}

fn run(source: String) -> Result<(), ParseError> {
    Scanner::new(&source).parser()?.parse().print();
    Ok(())
}
