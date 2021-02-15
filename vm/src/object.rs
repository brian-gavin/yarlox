use std::fmt;

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "\"{}\"", s),
        }
    }
}
