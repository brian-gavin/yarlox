use {
    environment::Environment,
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
    LoxFunction(Rc<LoxFunction>),
    LoxClass(Rc<LoxClass>),
    LoxInstance(Rc<RefCell<LoxInstance>>),
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    declaration: Stmt,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool,
}

#[derive(Clone, Debug)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, LoxFunction>,
}

#[derive(Clone, Debug)]
pub struct LoxInstance {
    class: Rc<LoxClass>,
    fields: HashMap<String, LoxType>,
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
            LoxType::LoxFunction(func) => {
                if let Stmt::Function { name, .. } = &func.declaration {
                    format!("<fn {}>", name.lexeme)
                } else {
                    panic!("LoxType::Function::Stmt is not type Function.");
                }
            }
            LoxType::LoxClass(class) => class.name.clone(),
            LoxType::LoxInstance(instance) => {
                format!("{} instance", instance.borrow().class.name.clone())
            }
        }
    }

    pub fn call(&self, intr: &mut Interpreter, arguments: &[LoxType]) -> Result<LoxType, String> {
        match self {
            LoxType::BuiltinFnClock => {
                builtin_fn_clock().map_err(|e| format!("BuiltinClock failed: {}", e))
            }
            LoxType::LoxFunction(func) => func.call(intr, arguments),
            LoxType::LoxClass(class) => {
                let instance = LoxInstance::new(class.clone());
                let instance = LoxType::LoxInstance(Rc::new(RefCell::new(instance)));
                if let Some(initializer) = class.get_method("init") {
                    initializer.bind(instance.clone()).call(intr, arguments)?;
                }
                Ok(instance)
            }
            _ => Err(String::from("Can only call functions and classes.")),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxType::BuiltinFnClock => 0,
            LoxType::LoxFunction(func) => func.arity(),
            LoxType::LoxClass(class) => {
                if let Some(initializer) = class.get_method("init") {
                    initializer.arity()
                } else {
                    0
                }
            }
            _ => panic!(
                "Checking arity of type that is not a function or class: {:#?}.",
                self
            ),
        }
    }
}

impl LoxFunction {
    pub fn new(declaration: Stmt, closure: Rc<RefCell<Environment>>, is_initializer: bool) -> Self {
        Self {
            declaration,
            closure,
            is_initializer,
        }
    }

    pub fn bind(&self, instance: LoxType) -> Self {
        let mut closure = Environment::from(self.closure.clone());
        closure.define(String::from("this"), instance);
        let declaration = self.declaration.clone();
        Self {
            declaration,
            closure: Rc::new(RefCell::new(closure)),
            is_initializer: self.is_initializer,
        }
    }

    pub fn arity(&self) -> usize {
        if let Stmt::Function { params, .. } = &self.declaration {
            params.len()
        } else {
            panic!("LoxType::Function::Stmt is not type Function.")
        }
    }

    pub fn call(&self, intr: &mut Interpreter, arguments: &[LoxType]) -> Result<LoxType, String> {
        if let Stmt::Function { params, body, .. } = &self.declaration {
            let mut env = Environment::from(self.closure.clone());
            for (i, param) in params.iter().enumerate() {
                env.define(param.lexeme.clone(), arguments[i].clone());
            }
            intr.execute_block(body, env)
                .map_err(|e| e.message())
                .map(|ret| {
                    if self.is_initializer {
                        let tmp_this_tok = Token::build().lexeme(String::from("this")).finalize();
                        self.closure.borrow().get_at(0, &tmp_this_tok).unwrap()
                    } else if let ExecuteReturn::Return(val) = ret {
                        val
                    } else {
                        LoxType::Nil
                    }
                })
        } else {
            panic!("LoxType::Function::Stmt is not type Function.")
        }
    }
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, LoxFunction>) -> Self {
        Self { name, methods }
    }

    pub fn get_method(&self, name: &str) -> Option<&LoxFunction> {
        self.methods.get(name)
    }
}

impl LoxInstance {
    pub fn new(class: Rc<LoxClass>) -> Self {
        LoxInstance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(instance: &LoxType, prop: &Token) -> Result<LoxType, RuntimeError> {
        if let LoxType::LoxInstance(instance) = instance {
            if let Some(value) = instance.borrow().fields.get(prop.lexeme.as_str()) {
                return Ok(value.clone());
            }
            if let Some(method) = instance.borrow().class.get_method(prop.lexeme.as_str()) {
                let method = method.bind(LoxType::LoxInstance(instance.clone()));
                let method = LoxType::LoxFunction(Rc::new(method));
                return Ok(method);
            }
            Err(RuntimeError {
                token: prop.clone(),
                msg: format!("Undefined property {}.", prop.lexeme),
            })
        } else {
            Err(RuntimeError {
                token: prop.clone(),
                msg: String::from("Only instances have properties."),
            })
        }
    }

    pub fn set(&mut self, prop: &Token, value: LoxType) {
        self.fields.insert(prop.lexeme.clone(), value);
    }
}

fn builtin_fn_clock() -> Result<LoxType, time::SystemTimeError> {
    let current_time = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)?
        .as_secs_f64();
    Ok(LoxType::Number(current_time))
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
