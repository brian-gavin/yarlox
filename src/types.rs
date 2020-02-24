use {
    environment::Environment,
    error::LoxErrorTrait,
    interpreter::{ExecuteReturn, Interpreter},
    std::{cell::RefCell, error::Error, rc::Rc, time},
    stmt::Stmt,
};

#[derive(Clone, Debug)]
pub enum LoxType<'a> {
    Number(f64),
    LoxString(String),
    Boolean(bool),
    Nil,
    BuiltinFnClock,
    LoxFunction {
        declaration: &'a Stmt,
        closure: Rc<RefCell<Environment<'a>>>,
    },
}

impl<'a> LoxType<'a> {
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
            LoxType::LoxFunction {
                declaration: Stmt::Function { name, .. },
                ..
            } => format!("<fn {}>", name.lexeme),
            LoxType::LoxFunction { .. } => panic!("LoxType::Function::Stmt is not type Function."),
        }
    }

    pub fn call(
        &self,
        intr: &mut Interpreter<'a>,
        arguments: &[Rc<LoxType<'a>>],
    ) -> Result<Rc<LoxType<'a>>, String> {
        match self {
            LoxType::BuiltinFnClock => {
                builtin_fn_clock().map_err(|e| format!("BuiltinClock failed: {}", e.description()))
            }
            LoxType::LoxFunction {
                declaration: &Stmt::Function { params, body, .. },
                closure,
            } => {
                let mut env = Environment::from(closure.clone());
                for (i, param) in params.iter().enumerate() {
                    env.define(param.lexeme.clone(), arguments[i].clone());
                }
                intr.execute_block(&body, env)
                    .map_err(|e| e.message())
                    .map(|ret| match ret {
                        ExecuteReturn::Return(val) => val,
                        _ => Rc::new(LoxType::Nil),
                    })
            }
            LoxType::LoxFunction { .. } => panic!("LoxType::Function::Stmt is not type Function."),
            _ => Err(String::from("Can only call functions and classes.")),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxType::BuiltinFnClock => 0,
            LoxType::LoxFunction {
                declaration: Stmt::Function { params, .. },
                ..
            } => params.len(),
            LoxType::LoxFunction { .. } => panic!("LoxType::Function::Stmt is not type Function."),
            _ => panic!("Checking arity of type that is not a function or class."),
        }
    }
}

fn builtin_fn_clock<'a>() -> Result<Rc<LoxType<'a>>, time::SystemTimeError> {
    let current_time = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)?
        .as_secs_f64();
    Ok(Rc::new(LoxType::Number(current_time)))
}

impl<'a> PartialEq for LoxType<'a> {
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
