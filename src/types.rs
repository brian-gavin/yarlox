use {
    interpreter::Interpreter,
    std::{error::Error, rc::Rc, time},
};

#[derive(Clone, Debug)]
pub enum LoxType {
    Number(f64),
    LoxString(String),
    Boolean(bool),
    Nil,
    BuiltinFnClock,
}

impl LoxType {
    pub fn is_truthy(&self) -> bool {
        match *self {
            LoxType::Nil => false,
            LoxType::Boolean(b) => b,
            _ => true,
        }
    }

    pub fn stringify(&self) -> String {
        match self {
            LoxType::LoxString(s) => s.clone(),
            LoxType::Number(n) => n.to_string(),
            LoxType::Boolean(b) => b.to_string(),
            LoxType::Nil => "nil".to_string(),
            LoxType::BuiltinFnClock => "<native fn>".to_string(),
        }
    }

    pub fn call(
        &self,
        _intr: &Interpreter,
        _arguments: &[Rc<LoxType>],
    ) -> Result<Rc<LoxType>, String> {
        match self {
            LoxType::BuiltinFnClock => {
                builtin_fn_clock().map_err(|e| format!("BuiltinClock failed: {}", e.description()))
            }
            _ => Err(String::from("Can only call functions and classes.")),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxType::BuiltinFnClock => 0,
            _ => panic!("Checking arity of type that is not a function or class."),
        }
    }
}

fn builtin_fn_clock() -> Result<Rc<LoxType>, time::SystemTimeError> {
    let current_time = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)?
        .as_secs_f64();
    Ok(Rc::new(LoxType::Number(current_time)))
}

impl PartialEq for LoxType {
    fn eq(&self, other: &LoxType) -> bool {
        use types::LoxType::*;

        match (self, other) {
            (Number(n1), Number(n2)) => n1 == n2,
            (LoxString(s1), LoxString(s2)) => s1 == s2,
            (Boolean(b1), Boolean(b2)) => b1 == b2,
            (Nil, Nil) => true,
            (Nil, _) => false,
            (_, Nil) => false,
            (_, _) => false,
        }
    }
}
