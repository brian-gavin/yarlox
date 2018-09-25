use {
    environment::Environment,
    expr::{Expr, Expr::*},
    std::rc::Rc,
    stmt::{Stmt, Stmt::*},
    token::Token,
    types::LoxType,
    visit::{expr, stmt},
    RuntimeError,
};

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), RuntimeError> {
        for stmt in stmts.iter() {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        use visit::stmt::Visitable;
        stmt.accept(self)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Rc<LoxType>, RuntimeError> {
        use visit::expr::Visitable;
        expr.accept(self)
    }

    fn evaluate_unary(&mut self, op: &Token, right: &Expr) -> Result<Rc<LoxType>, RuntimeError> {
        use {
            token::TokenType::{Bang, Minus},
            types::LoxType::{Boolean, Number},
        };

        let right = self.evaluate(right)?;
        let right = right.as_ref();
        let res = match (op.ttype, right) {
            (Minus, Number(n)) => Number(-n),
            (Minus, _) => return Err(self.number_error(op.clone())),
            (Bang, right @ _) => Boolean(!right.is_truthy()),
            _ => panic!("Unary operator that is neither Bang nor Minus"),
        };
        Ok(Rc::new(res))
    }

    fn evaluate_binary(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<Rc<LoxType>, RuntimeError> {
        use {
            token::TokenType::{
                BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash,
                Star,
            },
            types::LoxType::{Boolean, LoxString, Number},
        };

        let left = self.evaluate(left)?;
        let left = left.as_ref();
        let right = self.evaluate(right)?;
        let right = right.as_ref();

        let res = match (left, op.ttype, right) {
            (Number(n1), Star, Number(n2)) => Number(n1 * n2),
            (Number(n1), Slash, Number(n2)) => {
                if n2 != &0.0 {
                    Number(n1 / n2)
                } else {
                    return Err(self.error(op.clone(), "Divide by zero"));
                }
            }
            (Number(n1), Minus, Number(n2)) => Number(n1 - n2),
            (Number(n1), Plus, Number(n2)) => Number(n1 + n2),
            (LoxString(s), Plus, Number(n)) => LoxString(s.clone() + &n.to_string()),
            (Number(n), Plus, LoxString(s)) => LoxString(n.to_string() + &s),
            (LoxString(s1), Plus, LoxString(s2)) => LoxString(s1.clone() + &s2),
            (Number(n1), Greater, Number(n2)) => Boolean(n1 > n2),
            (Number(n1), GreaterEqual, Number(n2)) => Boolean(n1 >= n2),
            (Number(n1), Less, Number(n2)) => Boolean(n1 < n2),
            (Number(n1), LessEqual, Number(n2)) => Boolean(n1 <= n2),
            (ref o1 @ _, EqualEqual, ref o2 @ _) => Boolean(o1 == o2),
            (ref o1 @ _, BangEqual, ref o2 @ _) => Boolean(o1 != o2),
            (_, Star, _)
            | (_, Slash, _)
            | (_, Minus, _)
            | (_, Greater, _)
            | (_, GreaterEqual, _)
            | (_, Less, _)
            | (_, LessEqual, _) => return Err(self.number_error(op.clone())),
            (_, Plus, _) => {
                return Err(self.error(op.clone(), "Operators must be numbers or strings"))
            }
            (_, _, _) => return Err(self.error(op.clone(), "Unexpected binary expression")),
        };
        Ok(Rc::new(res))
    }

    fn number_error(&self, token: Token) -> RuntimeError {
        self.error(token, "Operands must be numbers.")
    }

    fn error(&self, token: Token, msg: &str) -> RuntimeError {
        let msg = msg.to_string();
        RuntimeError { token, msg }
    }
}

impl expr::Visitor<Result<Rc<LoxType>, RuntimeError>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<Rc<LoxType>, RuntimeError> {
        use types::LoxType::*;

        let res = match expr {
            StringLiteral(s) => Rc::new(LoxString(s.clone())),
            NumberLiteral(n) => Rc::new(Number(*n)),
            NilLiteral => Rc::new(Nil),
            TrueLiteral => Rc::new(Boolean(true)),
            FalseLiteral => Rc::new(Boolean(false)),
            Grouping(grouped) => self.evaluate(grouped)?,
            Unary { op, right } => self.evaluate_unary(op, right)?,
            Binary { left, op, right } => self.evaluate_binary(left, op, right)?,
            Variable { name } => self.environment.get(name)?,
            Assign { name, value } => {
                let value = self.evaluate(value)?;
                self.environment.assign(name, value.clone())?;
                value
            }
        };
        Ok(res)
    }
}

impl stmt::Visitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Print(expr) => {
                println!("{}", self.evaluate(expr)?.stringify());
            }
            Expression(expr) => {
                self.evaluate(expr)?;
            }
            Var { name, initializer } => {
                let val = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Rc::new(LoxType::Nil),
                };
                self.environment.define(name.lexeme.clone(), val);
                debug!("defining {}: env: {:?}", name.lexeme, self.environment);
            }
        }

        Ok(())
    }
}
