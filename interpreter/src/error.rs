use {
    std::{error::Error, fmt, process},
    token,
};

pub trait LoxErrorTrait
where
    Self: Error + Sized,
{
    fn report(&self) {
        eprintln!("{}", self);
    }

    fn message(&self) -> String {
        format!("{}", self)
    }

    fn report_exit(&self) -> ! {
        self.report();
        process::exit(0)
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub line: u32,
    pub e_type: String,
    pub msg: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] Error {}: {}", self.line, self.e_type, self.msg)
    }
}

impl Error for ParseError {}

impl LoxErrorTrait for ParseError {}

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
    pub msg: String,
    pub token: token::Token,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] Runtime Error: {}", self.token.line, self.msg)
    }
}

impl Error for RuntimeError {}

impl LoxErrorTrait for RuntimeError {}

#[derive(Debug)]
pub enum LoxError {
    ParseError(ParseError),
    RuntimeError(RuntimeError),
}

impl From<RuntimeError> for LoxError {
    fn from(e: RuntimeError) -> LoxError {
        LoxError::RuntimeError(e)
    }
}

impl From<ParseError> for LoxError {
    fn from(e: ParseError) -> LoxError {
        LoxError::ParseError(e)
    }
}
impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LoxError::*;
        match self {
            RuntimeError(e) => e.fmt(f),
            ParseError(e) => e.fmt(f),
        }
    }
}
impl Error for LoxError {}
impl LoxErrorTrait for LoxError {}
