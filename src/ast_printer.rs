use {
    expr::Expr,
    expr::Expr::*,
    visit::expr::{Visitable, Visitor},
};

pub struct Printer;

impl Printer {
    pub fn print(&self, expr: &Expr) -> String {
        self.visit_expr(expr)
    }

    fn parenthize(&self, name: &str, exprs: Vec<&Expr>) -> String {
        let mut rv = String::new();
        rv.push('(');
        rv.push_str(name);
        for expr in exprs.iter() {
            rv.push(' ');
            rv.push_str(expr.accept(self).as_str());
        }
        rv.push(')');
        rv
    }
}

impl Visitor<String> for Printer {
    fn visit_expr(&self, expr: &Expr) -> String {
        match expr {
            NilLiteral => "nil".to_string(),
            NumberLiteral(n) => n.to_string(),
            StringLiteral(s) => s.clone(),
            Grouping(e) => self.parenthize("grouping", vec![e]),
            Unary { op, right } => self.parenthize(&op.lexeme, vec![right]),
            Binary { left, op, right } => self.parenthize(&op.lexeme, vec![left, right]),
            FalseLiteral => "false".to_string(),
            TrueLiteral => "true".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        ast_printer::Printer,
        expr::Expr,
        expr::Expr::*,
        token::{Token, TokenType},
    };
    #[test]
    fn test_pretty_print() {
        let ast = Binary {
            left: Box::new(NumberLiteral(1.23)),
            op: Token::build()
                .lexeme_str("+")
                .ttype(TokenType::Plus)
                .finalize(),
            right: Box::new(Unary {
                op: Token::build()
                    .lexeme_str("-")
                    .ttype(TokenType::Minus)
                    .finalize(),
                right: Box::new(NumberLiteral(2.0)),
            }),
        };
        let expected = "(+ 1.23 (- 2))";
        let printer = Printer;
        assert_eq!(expected, printer.print(&ast));
    }
}
