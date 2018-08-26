use {
    expr::{Expr, Expr::*},
    token::Token,
    types::LoxType,
    visit::{Visitable, Visitor},
};

pub struct Interpreter;

impl Interpreter {
    fn evaluate(&self, expr: &Expr) -> LoxType {
        expr.accept(self)
    }

    fn evaluate_unary(&self, op: &Token, right: &Expr) -> LoxType {
        use {
            token::TokenType::{Bang, Minus},
            types::LoxType::{Boolean, Nil, Number},
        };

        let right = self.evaluate(right);
        match (op.ttype, right) {
            (Minus, Number(n)) => LoxType::Number(-n),
            (Minus, _) => Nil, // todo: error handling
            (Bang, right @ _) => Boolean(!right.is_truthy()),
            _ => Nil,
        }
    }

    fn evaluate_binary(&self, left: &Expr, op: &Token, right: &Expr) -> LoxType {
        use {
            token::TokenType::{
                BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash,
                Star,
            },
            types::LoxType::{Boolean, LoxString, Nil, Number},
        };

        let left = self.evaluate(left);
        let right = self.evaluate(right);

        match (left, op.ttype, right) {
            (Number(n1), Star, Number(n2)) => Number(n1 * n2),
            (Number(n1), Slash, Number(n2)) => Number(n1 / n2),
            (Number(n1), Minus, Number(n2)) => Number(n1 - n2),
            (Number(n1), Plus, Number(n2)) => Number(n1 + n2),
            (LoxString(s1), Plus, LoxString(s2)) => LoxString(s1 + &s2),
            (Number(n1), Greater, Number(n2)) => Boolean(n1 > n2),
            (Number(n1), GreaterEqual, Number(n2)) => Boolean(n1 >= n2),
            (Number(n1), Less, Number(n2)) => Boolean(n1 < n2),
            (Number(n1), LessEqual, Number(n2)) => Boolean(n1 <= n2),
            (ref o1 @ _, EqualEqual, ref o2 @ _) => Boolean(o1 == o2),
            (ref o1 @ _, BangEqual, ref o2 @ _) => Boolean(o1 != o2),
            (_, _, _) => Nil, // TODO: error handling
        }
    }
}

impl Visitor<LoxType> for Interpreter {
    fn visit_expr(&self, expr: &Expr) -> LoxType {
        use types::LoxType::*;

        match expr {
            StringLiteral(s) => LoxString(s.clone()),
            NumberLiteral(n) => Number(*n),
            NilLiteral => Nil,
            TrueLiteral => Boolean(true),
            FalseLiteral => Boolean(false),
            Grouping(grouped) => self.evaluate(grouped),
            Unary { op, right } => self.evaluate_unary(op, right),
            Binary { left, op, right } => self.evaluate_binary(left, op, right),
        }
    }
}
