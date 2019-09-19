use {
    error::RuntimeError,
    std::{collections::HashMap, rc::Rc},
    token::Token,
    types::LoxType,
};

#[derive(Debug)]
pub struct Environment {
    scopes: Vec<HashMap<String, Rc<LoxType>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn define(&mut self, name: String, value: Rc<LoxType>) {
        self.scopes
            .last_mut()
            .expect("Empty environment")
            .insert(name, value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<Rc<LoxType>, RuntimeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(&name.lexeme) {
                return Ok(val.clone());
            }
        }
        Err(Environment::undefined_variable(name))
    }

    pub fn assign(&mut self, name: &Token, value: Rc<LoxType>) -> Result<(), RuntimeError> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(&name.lexeme) {
                scope.insert(name.lexeme.clone(), value.clone());
                return Ok(());
            }
        }
        Err(Environment::undefined_variable(name))
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("Popped from empty environment");
    }

    fn undefined_variable(tok: &Token) -> RuntimeError {
        RuntimeError {
            msg: format!("Undefined variable '{}'.", tok.lexeme),
            token: tok.clone(),
        }
    }
}
