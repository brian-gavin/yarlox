use {
    error::RuntimeError,
    std::{cell::RefCell, collections::HashMap, rc::Rc},
    token::Token,
    types::LoxType,
};

#[derive(Debug)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    scope: HashMap<String, Rc<LoxType>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            enclosing: None,
            scope: HashMap::new(),
        }
    }

    pub fn from(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            enclosing: Some(enclosing),
            scope: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Rc<LoxType>) {
        self.scope.insert(name, value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<Rc<LoxType>, RuntimeError> {
        if let Some(val) = self.scope.get(&name.lexeme) {
            Ok(val.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(Environment::undefined_variable(name))
        }
    }

    pub fn assign(&mut self, name: &Token, value: Rc<LoxType>) -> Result<(), RuntimeError> {
        if self.scope.contains_key(&name.lexeme) {
            self.scope.insert(name.lexeme.clone(), value.clone());
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(Environment::undefined_variable(name))
        }
    }

    fn undefined_variable(tok: &Token) -> RuntimeError {
        RuntimeError {
            msg: format!("Undefined variable '{}'.", tok.lexeme),
            token: tok.clone(),
        }
    }
}
