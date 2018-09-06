#[derive(Clone, Debug)]
pub enum LoxType {
    Number(f64),
    LoxString(String),
    Boolean(bool),
    Nil,
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
        }
    }
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
