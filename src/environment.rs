use {
    error::RuntimeError,
    std::{cell::RefCell, collections::HashMap, rc::Rc},
    token::Token,
    types::LoxType,
};

#[derive(Debug)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    scope: HashMap<String, LoxType>,
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

    pub fn define(&mut self, name: String, value: LoxType) {
        debug!("define({}, {:#?})", name, value);
        self.scope.insert(name, value.clone());
        debug!("{:#?}", self);
    }

    pub fn get(&self, name: &Token) -> Result<LoxType, RuntimeError> {
        debug!("get({})", name.lexeme);
        if let Some(val) = self.scope.get(&name.lexeme) {
            Ok(val.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(Environment::undefined_variable(name))
        }
    }

    // TODO: does this need to be Rc or can be Weak?
    fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<Environment>>> {
        let mut environment = self.enclosing.clone();
        for _ in 1..distance {
            environment = environment.and_then(|env| env.borrow().enclosing.clone());
        }
        debug!("ancestor({}) -> {:#?}", distance, environment);
        environment
    }

    pub fn get_at(&self, distance: usize, name: &Token) -> Result<LoxType, RuntimeError> {
        debug!("get_at({}, {})", distance, name.lexeme);
        if distance == 0 {
            self.get(name)
        } else {
            self.ancestor(distance).map_or_else(
                || Err(Environment::undefined_variable(name)),
                |ancestor| ancestor.borrow().get(name),
            )
        }
    }

    pub fn assign(&mut self, name: &Token, value: LoxType) -> Result<(), RuntimeError> {
        debug!("assign({}, {:#?})", name.lexeme, value);
        if self.scope.contains_key(&name.lexeme) {
            self.scope.insert(name.lexeme.clone(), value.clone());
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(Environment::undefined_variable(name))
        }
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        name: &Token,
        value: LoxType,
    ) -> Result<(), RuntimeError> {
        if distance == 0 {
            self.assign(name, value)
        } else {
            self.ancestor(distance).map_or_else(
                || Err(Environment::undefined_variable(name)),
                |ancestor| ancestor.borrow_mut().assign(name, value),
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
