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
    Runtime(String),
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpretError::Runtime(msg) => write!(f, "{}", msg),
            InterpretError::CompileTime => write!(f, "{:?}", self),
        }
    }
}

impl Error for InterpretError {}

pub type InterpretResult = Result<(), InterpretError>;

macro_rules! binary_op {
    ($vm:ident, $op:tt) => {
        {
            match ($vm.stack.last(), $vm.stack.get($vm.stack.len() - 2)) {
                (Some(Value::Number(_)), Some(Value::Number(_))) => {
                    let b = $vm.stack.pop().expect("empty stack!");
                    let a = $vm.stack.last_mut().expect("empty stack!");
                    *a = *a $op b;
                }
                (None, _) | (_, None) => panic!("empty stack!"),
                _ => return Err($vm.runtime_error("Operands must be numbers.")),
            }
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
                Nil => self.stack.push(Value::Nil),
                True => self.stack.push(Value::Boolean(true)),
                False => self.stack.push(Value::Boolean(false)),
                Add => binary_op!(self, +),
                Subtract => binary_op!(self, -),
                Multiply => binary_op!(self, *),
                Divide => binary_op!(self, /),
                Negate => {
                    if let Value::Number(v) = self.stack.last_mut().expect("empty stack!") {
                        *v = -*v;
                    } else {
                        return Err(self.runtime_error("Operand must be a number"));
                    }
                }
                Return => {
                    println!("{}", self.stack.pop().expect("empty stack!"));
                    return Ok(());
                }
            }
        }
    }

    fn runtime_error(&self, msg: &str) -> InterpretError {
        let line = self.chunk.lines().get(&(self.ip - 1)).unwrap();
        InterpretError::Runtime(format!("{}\n[line {}] in script", msg, line))
    }
}
