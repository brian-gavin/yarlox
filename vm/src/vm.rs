use crate::{
    chunk::{Chunk, OpCode},
    value::Value,
};
use std::{
    error::Error,
    fmt::{self, Display},
};
pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum InterpretError {
    CompileTime,
    Runtime,
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for InterpretError {}

pub type InterpretResult = Result<(), InterpretError>;

macro_rules! binary_op {
    ($vm:ident, $op:tt) => {
        {
            let b = $vm.stack.pop().expect("empty stack!");
            let a = $vm.stack.last_mut().expect("empty stack!");
            *a = *a $op b;
        }
    };
}

impl Vm {
    pub fn new(chunk: Chunk) -> Vm {
        Vm {
            chunk,
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        use OpCode::*;
        loop {
            let op = self.chunk.fetch(self.ip);
            debug!("stack: {:?}", self.stack);
            debug!("op: {:?}", op);
            self.ip += 1;
            match op {
                Constant(idx) => {
                    let constant = self.chunk.get_constant(*idx as _);
                    self.stack.push(*constant);
                }
                Add => binary_op!(self, +),
                Subtract => binary_op!(self, -),
                Multiply => binary_op!(self, *),
                Divide => binary_op!(self, /),
                Negate => {
                    let v = self.stack.last_mut().expect("empty stack!");
                    *v = -*v;
                }
                Return => {
                    println!("{}", self.stack.pop().expect("empty stack!"));
                    return Ok(());
                }
            }
        }
    }
}
