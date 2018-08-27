use {
    expr::{Expr, Expr::*},
    token::Token,
    types::LoxType,
    visit::{Visitable, Visitor},
    RuntimeError,
};

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(self, expr: &Expr) -> Result<(), RuntimeError> {
        Ok(println!("{}", self.evaluate(expr)?.stringify()))
    }

    fn evaluate(&self, expr: &Expr) -> Result<LoxType, RuntimeError> {
        expr.accept(self)
    }

    fn evaluate_unary(&self, op: &Token, right: &Expr) -> Result<LoxType, RuntimeError> {
        use {
            token::TokenType::{Bang, Minus},
            types::LoxType::{Boolean, Number},
        };

        let right = self.evaluate(right)?;
        match (op.ttype, right) {
            (Minus, Number(n)) => Ok(Number(-n)),
            (Minus, _) => Err(self.number_error(op.clone())),
            (Bang, right @ _) => Ok(Boolean(!right.is_truthy())),
            _ => panic!("Unary operator that is neither Bang nor Minus"),
        }
    }

    fn evaluate_binary(
        &self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<LoxType, RuntimeError> {
        use {
            token::TokenType::{
                BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash,
                Star,
            },
            types::LoxType::{Boolean, LoxString, Number},
        };

        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        let res = match (left, op.ttype, right) {
            (Number(n1), Star, Number(n2)) => Number(n1 * n2),
            (Number(n1), Slash, Number(n2)) => {
                if n2 != 0.0 {
                    Number(n1 / n2)
                } else {
                    return Err(self.error(op.clone(), "Divide by zero"));
                }
            }
            (Number(n1), Minus, Number(n2)) => Number(n1 - n2),
            (Number(n1), Plus, Number(n2)) => Number(n1 + n2),
            (LoxString(s), Plus, Number(n)) => LoxString(s + &n.to_string()),
            (Number(n), Plus, LoxString(s)) => LoxString(n.to_string() + &s),
            (LoxString(s1), Plus, LoxString(s2)) => LoxString(s1 + &s2),
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
        Ok(res)
    }

    fn number_error(&self, token: Token) -> RuntimeError {
        self.error(token, "Operands must be numbers.")
    }

    fn error(&self, token: Token, msg: &str) -> RuntimeError {
        let msg = msg.to_string();
        RuntimeError { token, msg }
    }
}

impl Visitor<Result<LoxType, RuntimeError>> for Interpreter {
    fn visit_expr(&self, expr: &Expr) -> Result<LoxType, RuntimeError> {
        use types::LoxType::*;

        let res = match expr {
            StringLiteral(s) => LoxString(s.clone()),
            NumberLiteral(n) => Number(*n),
            NilLiteral => Nil,
            TrueLiteral => Boolean(true),
            FalseLiteral => Boolean(false),
            Grouping(grouped) => self.evaluate(grouped)?,
            Unary { op, right } => self.evaluate_unary(op, right)?,
            Binary { left, op, right } => self.evaluate_binary(left, op, right)?,
        };
        Ok(res)
    }
}
