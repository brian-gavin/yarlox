use {
    std::{collections::HashMap, rc::Rc},
    token::Token,
    types::LoxType,
    RuntimeError,
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
            None => Err(RuntimeError {
                msg: format!("Undefined variable '{}'.", name.lexeme),
                token: name.clone(),
            }),
        }
    }
}
