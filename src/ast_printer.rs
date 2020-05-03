use {expr::Expr, expr::ExprKind::*};

pub struct Printer;

impl Printer {
    pub fn print(&mut self, expr: &Expr) -> String {
        self.eval(expr)
    }

    fn parenthize(&mut self, name: &str, exprs: Vec<&Expr>) -> String {
        let mut rv = String::new();
        rv.push('(');
        rv.push_str(name);
        for expr in exprs.iter() {
            rv.push(' ');
            rv.push_str(self.eval(expr).as_str());
        }
        rv.push(')');
        rv
    }

    fn eval(&mut self, expr: &Expr) -> String {
        match &expr.kind {
            NilLiteral => "nil".to_string(),
            NumberLiteral(n) => n.to_string(),
            StringLiteral(s) => s.clone(),
            Grouping(e) => self.parenthize("grouping", vec![&e]),
            Unary { op, right } => self.parenthize(&op.lexeme, vec![&right]),
            Logical { left, op, right } | Binary { left, op, right } => {
                self.parenthize(&op.lexeme, vec![&left, &right])
            }
            FalseLiteral => "false".to_string(),
            TrueLiteral => "true".to_string(),
            Variable { name } => format!("variable: {}", name.lexeme),
            Assign { name, value } => format!("assign: {} to {}", self.eval(&value), name.lexeme),
            Call {
                arguments, callee, ..
            } => format!(
                "{}({})",
                self.eval(&callee),
                arguments
                    .iter()
                    .map(|e| self.eval(e))
                    .collect::<Vec<_>>()
                    .as_slice()
                    .join(",")
            ),
            Get { object, name } => format!("{}.{}", self.eval(&object), name.lexeme),
            Set { object, value, .. } => format!("{} = {}", self.eval(&object), self.eval(&value)),
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        ast_printer::Printer,
        expr::{Expr, ExprKind::*},
        token::{Token, TokenType},
    };
    #[test]
    fn test_pretty_print() {
        let ast = Expr::of(Binary {
            left: Box::new(Expr::of(NumberLiteral(1.23))),
            op: Token::build()
                .lexeme_str("+")
                .ttype(TokenType::Plus)
                .finalize(),
            right: Box::new(Expr::of(Unary {
                op: Token::build()
                    .lexeme_str("-")
                    .ttype(TokenType::Minus)
                    .finalize(),
                right: Box::new(Expr::of(NumberLiteral(2.0))),
            })),
        });
        let expected = "(+ 1.23 (- 2))";
        let mut printer = Printer;
        assert_eq!(expected, printer.print(&ast));
    }
}
