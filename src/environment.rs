use {
    error::RuntimeError,
    std::{collections::HashMap, rc::Rc},
    token::Token,
    types::LoxType,
};

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Rc<LoxType>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Rc<LoxType>) {
        self.values.insert(name, value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<Rc<LoxType>, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(val) => Ok(val.clone()),
            None => Err(Environment::undefined_variable(name)),
        }
    }

    pub fn assign(&mut self, name: &Token, value: Rc<LoxType>) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(())
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
