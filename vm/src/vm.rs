use crate::{
    chunk::{Chunk, OpCode},
    object::Object,
    value::Value,
};
use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display},
};
pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
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

macro_rules! number_binary_op {
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
    pub fn new() -> Vm {
        Vm {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        self.run()
    }

    fn read_string(&mut self, idx: usize) -> String {
        let s = self
            .chunk
            .get_mut_constant(idx as _)
            .take()
            .expect(&format!("vm::ReadString: None at idx {}", idx));
        if let Value::Object(Object::String(s)) = s {
            s
        } else {
            panic!("vm::ReadString: Value at idx {} was not a string", idx)
        }
    }

    fn run(&mut self) -> InterpretResult {
        use OpCode::*;
        loop {
            let op = self.chunk.fetch(self.ip);
            debug!("stack: {:?}", self.stack);
            debug!("op: {:?}", op);
            self.ip += 1;
            match op {
                Constant(idx) => {
                    let constant = self.chunk.get_mut_constant(idx as _);
                    self.stack
                        .push(constant.take().expect("took a constant that was taken"));
                }
                Nil => self.stack.push(Value::Nil),
                True => self.stack.push(Value::Boolean(true)),
                False => self.stack.push(Value::Boolean(false)),
                Pop => {
                    let _ = self.stack.pop().expect("empty stack!");
                }
                GetGlobal(idx) => {
                    let name = self.read_string(idx as _);
                    match self.globals.get(&name) {
                        Some(global_var) => {
                            // cloning is OK - for basic values, they are already cheaply clonable
                            // for later objects, they will probably be a Rc clone, or some other GC thing.
                            self.stack.push(global_var.clone())
                        }
                        None => {
                            return Err(InterpretError::Runtime(format!(
                                "Undefined variable '{}'.",
                                name
                            )));
                        }
                    }
                }
                DefineGlobal(idx) => {
                    let name = self.read_string(idx as _);
                    // in the book, this first peeks to insert then pops after.
                    // we cannot really do that, so we just pop it off now.
                    // hopefully this won't cause a headache later.
                    let val = self.stack.pop().expect("empty stack!");
                    self.globals.insert(name, val);
                    debug!("DefineGlobal::globals: {:?}", self.globals);
                }
                SetGlobal(idx) => {
                    let name = self.read_string(idx as _);
                    // Set is an expression and must leave a value on the stack, so do not pop.
                    let v = self.stack.last().expect("empty stack!").clone();
                    match self.globals.insert(name.clone(), v) {
                        None => {
                            self.globals.remove(&name);
                            return Err(InterpretError::Runtime(format!(
                                "Undefined variable '{}'.",
                                name,
                            )));
                        }
                        Some(_) => {}
                    }
                }
                Equal => {
                    let b = self.stack.pop().expect("empty stack!");
                    let a = self.stack.last_mut().expect("empty stack!");
                    *a = Value::Boolean(*a == b);
                }
                Add => {
                    let b = self.stack.pop().expect("empty stack!");
                    let a = self.stack.last_mut().expect("empty stack!");
                    match (a, b) {
                        (Value::Object(Object::String(a)), Value::Object(Object::String(b))) => {
                            *a = a.to_owned() + b.as_str();
                        }
                        (Value::Number(a), Value::Number(b)) => {
                            *a += b;
                        }
                        _ => {
                            return Err(
                                self.runtime_error("Operands must be two numbers or two strings.")
                            );
                        }
                    }
                }
                Greater => number_binary_op!(self, Boolean, >),
                Less => number_binary_op!(self, Boolean, <),
                Subtract => number_binary_op!(self, Number, -),
                Multiply => number_binary_op!(self, Number, *),
                Divide => number_binary_op!(self, Number, /),
                Not => {
                    let b = self.stack.last_mut().expect("empty stack!");
                    *b = Value::Boolean(b.is_falsey());
                }
                Negate => {
                    let v = self.stack.last_mut().expect("empty stack!");
                    *v = match &v {
                        Value::Number(n) => Value::Number(-n),
                        _ => return Err(self.runtime_error("Operand must be a number.")),
                    }
                }
                Print => {
                    println!("{}", self.stack.pop().expect("empty stack!"));
                }
                Return => {
                    // Exit interpreter
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
