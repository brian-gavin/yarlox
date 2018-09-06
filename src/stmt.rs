use {
    expr::Expr,
    token::Token,
    visit::stmt::{Visitable, Visitor},
};

#[derive(Debug)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var {
        name: Token,
        initializer: Option<Box<Expr>>,
    },
}

impl<T> Visitable<T> for Stmt {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> T {
        visitor.visit_stmt(self)
    }
}
