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
    parser::Parser,
    interpreter::Interpreter,
    std::{
        error::Error,
        fmt, fs, io,
        io::{BufRead, Read},
        process,
    },
};

trait LoxError
where
    Self: Error + Sized,
{
    fn report(&self) {
        eprintln!("{}", self);
    }

    fn message(&self) -> String {
        format!("{}", self)
    }

    fn report_exit(self) -> ! {
        self.report();
        process::exit(0)
    }
}

#[derive(Clone, Debug)]
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

impl Error for ParseError {}

impl LoxError for ParseError {}

impl ParseError {
    pub fn new(line: u32, e_type: String, msg: String) -> ParseError {
        ParseError {
            line: line,
            e_type: e_type,
            msg: msg,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeError {
    msg: String,
    token: token::Token,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] Runtime Error: {}", self.token.line, self.msg)
    }
}

impl Error for RuntimeError {}

impl LoxError for RuntimeError {}

#[derive(Debug)]
enum LoxErrorUnion {
    RuntimeError(RuntimeError),
    ParseError(ParseError),
}

impl fmt::Display for LoxErrorUnion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxErrorUnion::RuntimeError(e) => write!(f, "{}", e),
            LoxErrorUnion::ParseError(e) => write!(f, "{}", e),
        }
    }
}

impl From<RuntimeError> for LoxErrorUnion {
    fn from(e: RuntimeError) -> LoxErrorUnion {
        LoxErrorUnion::RuntimeError(e)
    }
}

impl From<ParseError> for LoxErrorUnion {
    fn from(e: ParseError) -> LoxErrorUnion {
        LoxErrorUnion::ParseError(e)
    }
}

impl Error for LoxErrorUnion {}

impl LoxError for LoxErrorUnion {}

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

fn run(source: String) -> Result<(), LoxErrorUnion> {
    let scanner = Scanner::new(&source);
    let ast = Parser::new(scanner)?.parse()?;
    Interpreter.interpret(&ast)?;
    Ok(())
}
