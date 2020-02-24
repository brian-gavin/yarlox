use {
    error::RuntimeError,
    std::{cell::RefCell, collections::HashMap, rc::Rc},
    token::Token,
    types::LoxType,
};

#[derive(Debug)]
pub struct Environment<'a> {
    enclosing: Option<Rc<RefCell<Environment<'a>>>>,
    scope: HashMap<String, Rc<LoxType<'a>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        Environment {
            enclosing: None,
            scope: HashMap::new(),
        }
    }

    pub fn from(enclosing: Rc<RefCell<Environment<'a>>>) -> Environment<'a> {
        Environment {
            enclosing: Some(enclosing),
            scope: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Rc<LoxType<'a>>) {
        self.scope.insert(name, value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<Rc<LoxType<'a>>, RuntimeError> {
        if let Some(val) = self.scope.get(&name.lexeme) {
            Ok(val.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(Environment::undefined_variable(name))
        }
    }

    fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<Environment<'a>>>>
    {
        let mut environment = self.enclosing.clone();
        for _ in 1..distance {
            environment = environment.and_then(|env| env.borrow().enclosing.clone());
        }
        environment
    }

    pub fn get_at(&self, distance: usize, name: &Token) -> Result<Rc<LoxType<'a>>, RuntimeError> {
        if distance == 0 {
            self.get(name)
        } else {
            self.ancestor(distance).map_or_else(
                || Err(Environment::undefined_variable(name)),
                |ancestor| ancestor.borrow().get(name),
            )
        }
    }

    pub fn assign(&mut self, name: &Token, value: Rc<LoxType<'a>>) -> Result<(), RuntimeError> {
        if self.scope.contains_key(&name.lexeme) {
            self.scope.insert(name.lexeme.clone(), value.clone());
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(Environment::undefined_variable(name))
        }
    }

    pub fn assign_at(&mut self, distance: usize, name: &Token, value: Rc<LoxType<'a>>) -> Result<(), RuntimeError> {
        if distance == 0 {
            self.assign(name, value)
        } else {
            self.ancestor(distance).map_or_else(
                || Err(Environment::undefined_variable(name)),
                |ancestor| ancestor.borrow_mut().assign(name, value)
            )
        }
    }

    fn undefined_variable(tok: &Token) -> RuntimeError {
        RuntimeError {
            msg: format!("Undefined variable '{}'.", tok.lexeme),
            token: tok.clone(),
        }
    }
}
