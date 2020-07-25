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
    ($vm:ident, $value_type:ident, $op:tt) => {{
        let b = $vm.stack.pop().expect("empty stack!");
        let a = $vm.stack.last_mut().expect("empty stack!");
        *a = match (&a, b) {
            (Value::Number(a), Value::Number(b)) => Value::$value_type(*a $op b),
            _ => return Err($vm.runtime_error("Operand must be a number.")),
        };
    }};
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
                Equal => {
                    let b = self.stack.pop().expect("empty stack!");
                    let a = self.stack.last_mut().expect("empty stack!");
                    *a = Value::Boolean(*a == b);
                }
                Greater => binary_op!(self, Boolean, >),
                Less => binary_op!(self, Boolean, <),
                Add => binary_op!(self, Number, +),
                Subtract => binary_op!(self, Number, -),
                Multiply => binary_op!(self, Number, *),
                Divide => binary_op!(self, Number, /),
                Not => {
                    let b = self.stack.last_mut().expect("empty stack!");
                    *b = !*b;
                }
                Negate => {
                    let v = self.stack.last_mut().expect("empty stack!");
                    match -*v {
                        Ok(value) => *v = value,
                        Err(msg) => return Err(self.runtime_error(msg.as_str())),
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
