use {
    environment::{Environment, EnvironmentEntry},
    error::{LoxErrorTrait, RuntimeError},
    interpreter::{ExecuteReturn, Interpreter},
    std::{cell::RefCell, collections::HashMap, rc::Rc, time},
    stmt::Stmt,
    token::Token,
};

#[derive(Clone, Debug)]
pub enum LoxType {
    Number(f64),
    LoxString(String),
    Boolean(bool),
    Nil,
    BuiltinFnClock,
    LoxFunction(LoxFunction),
    LoxClass(LoxClass),
    LoxInstance(LoxInstance),
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    declaration: Stmt,
    closure: Rc<RefCell<Environment>>,
}

#[derive(Clone, Debug)]
pub struct LoxClass {
    name: String,
}

#[derive(Clone, Debug)]
pub struct LoxInstance {
    class: EnvironmentEntry,
    fields: HashMap<String, Rc<RefCell<LoxType>>>,
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
            LoxType::LoxFunction(LoxFunction { declaration, .. }) => {
                if let Stmt::Function { name, .. } = declaration {
                    format!("<fn {}>", name.lexeme)
                } else {
                    panic!("LoxType::Function::Stmt is not type Function.");
                }
            }
            LoxType::LoxClass(LoxClass { name }) => name.clone(),
            LoxType::LoxInstance(LoxInstance { class, .. }) => {
                format!("{} instance", class.borrow().stringify())
            }
        }
    }

    pub fn call(
        callee: EnvironmentEntry,
        intr: &mut Interpreter,
        arguments: &[EnvironmentEntry],
    ) -> Result<EnvironmentEntry, String> {
        match &*callee.borrow() {
            LoxType::BuiltinFnClock => {
                builtin_fn_clock().map_err(|e| format!("BuiltinClock failed: {}", e))
            }
            LoxType::LoxFunction(LoxFunction {
                declaration:
                    Stmt::Function {
                        params, ref body, ..
                    },
                closure,
            }) => {
                let mut env = Environment::from(closure.clone());
                for (i, param) in params.iter().enumerate() {
                    env.define(param.lexeme.clone(), arguments[i].clone());
                }
                intr.execute_block(body, env)
                    .map_err(|e| e.message())
                    .map(|ret| match ret {
                        ExecuteReturn::Return(val) => val,
                        _ => Rc::new(RefCell::new(LoxType::Nil)),
                    })
            }
            LoxType::LoxClass(_) => {
                let instance = Rc::new(RefCell::new(LoxType::LoxInstance(LoxInstance::new(
                    callee.clone(),
                ))));
                Ok(instance)
            }
            _ => Err(String::from("Can only call functions and classes.")),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxType::BuiltinFnClock => 0,
            LoxType::LoxFunction(LoxFunction {
                declaration: Stmt::Function { params, .. },
                ..
            }) => params.len(),
            LoxType::LoxClass { .. } => 0,
            _ => panic!(
                "Checking arity of type that is not a function or class: {:#?}.",
                self
            ),
        }
    }
}

impl LoxFunction {
    pub fn new(declaration: Stmt, closure: Rc<RefCell<Environment>>) -> Self {
        Self {
            declaration,
            closure,
        }
    }
}

impl LoxClass {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl LoxInstance {
    pub fn new(class: EnvironmentEntry) -> Self {
        LoxInstance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, prop: &Token) -> Result<Rc<RefCell<LoxType>>, RuntimeError> {
        if let Some(value) = self.fields.get(prop.lexeme.as_str()) {
            Ok(value.clone())
        } else {
            Err(RuntimeError {
                token: prop.clone(),
                msg: format!("Undefined property {}.", prop.lexeme),
            })
        }
    }
}

fn builtin_fn_clock() -> Result<EnvironmentEntry, time::SystemTimeError> {
    let current_time = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)?
        .as_secs_f64();
    Ok(Rc::new(RefCell::new(LoxType::Number(current_time))))
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
