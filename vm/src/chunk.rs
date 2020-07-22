use crate::value::Value;
use std::{collections::HashMap, convert::TryInto, fmt};

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant(u8),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

#[derive(Debug)]
pub struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<Value>,
    lines: HashMap<usize, usize>,
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, code) in self.code.iter().enumerate() {
            write!(f, "{:#04} ", i)?;
            if i > 0 && self.lines.get(&i) == self.lines.get(&(i - 1)) {
                write!(f, "   | ")?
            } else {
                write!(f, "{:#4} ", self.lines.get(&i).unwrap())?;
            }
            write!(f, "{:?}", code)?;
            match code {
                OpCode::Constant(i) => {
                    writeln!(f, "        {:?}", self.constants.get(*i as usize))?
                }
                _ => writeln!(f, "")?,
            }
        }
        Ok(())
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: HashMap::new(),
        }
    }

    #[must_use = "Add the return value to this to a OpCode::Constant"]
    pub fn add_constant(&mut self, constant: Value) -> u8 {
        self.constants.push(constant);
        (self.constants.len() - 1).try_into().unwrap()
    }

    pub fn write_chunk(&mut self, chunk: OpCode, line: usize) {
        self.code.push(chunk);
        self.lines.insert(self.code.len() - 1, line);
    }

    pub fn fetch(&self, ip: usize) -> &OpCode {
        self.code.get(ip).expect("Out of bounds IP")
    }

    pub fn get_constant(&self, idx: usize) -> &Value {
        self.constants.get(idx).expect("Out of bound constant")
    }
}
