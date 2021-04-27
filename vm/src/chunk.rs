use crate::value::Value;
use std::{collections::HashMap, convert::TryInto, fmt};

/// OpCode. For type safety reasons, a difference between this and the book is that multi-byte opcodes
/// are just a single variant with a byte member.
/// In the book, these are two byte: OP_CONSTANT <byte> but in this, it's just Constant(<byte>).
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant(u8),
    Nil,
    True,
    False,
    Pop,
    GetGlobal(u8),
    DefineGlobal(u8),
    SetGlobal(u8),
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Return,
}

#[derive(Debug)]
pub struct Chunk {
    code: Vec<OpCode>,
    // this is a Vec<Option> because later on, these will be take()'n by the vm
    constants: Vec<Option<Value>>,
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
    pub fn add_constant(
        &mut self,
        constant: Option<Value>,
    ) -> Result<u8, <usize as TryInto<u8>>::Error> {
        self.constants.push(constant);
        (self.constants.len() - 1).try_into()
    }

    pub fn write_chunk(&mut self, chunk: OpCode, line: usize) {
        self.code.push(chunk);
        self.lines.insert(self.code.len() - 1, line);
    }

    pub fn fetch(&self, ip: usize) -> OpCode {
        *self.code.get(ip).expect("Out of bounds IP")
    }

    pub fn get_mut_constant(&mut self, idx: usize) -> &mut Option<Value> {
        self.constants.get_mut(idx).expect("Out of bound constant")
    }

    pub fn lines(&self) -> &HashMap<usize, usize> {
        &self.lines
    }
}
