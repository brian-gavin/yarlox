#[macro_use]
extern crate lazy_static;

mod ast_printer;
mod expr;
mod parser;
mod scanner;
mod token;
mod visit;

use {
    ast_printer::Printer,
    scanner::Scanner,
    std::fmt,
    std::fs,
    std::io,
    std::io::{BufRead, Read},
    std::process,
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
}

pub fn report_exit(e: ParseError) -> ! {
    e.report();
    process::exit(0)
}

pub fn run_file(file_name: &str) -> Result<(), io::Error> {
    let mut file = fs::File::open(file_name)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    run(source);
    Ok(())
}

pub fn run_prompt() -> Result<(), io::Error> {
    let handle = io::stdin();
    loop {
        let mut source = String::new();
        match handle.lock().read_line(&mut source)? {
            0 => break,
            _ => run(source),
        }
    }
    Ok(())
}

fn run(source: String) {
    match run_err(source) {
        Ok(_) => (),
        Err(e) => report_exit(e),
    }
}

fn run_err(source: String) -> Result<(), ParseError> {
    Scanner::new(&source).parser().parse().print();
    Ok(())
}
