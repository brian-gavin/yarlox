use {
    expr::Expr,
    visit::stmt::{Visitable, Visitor},
};

#[derive(Debug)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
}

impl<T> Visitable<T> for Stmt {
    fn accept(&self, visitor: &impl Visitor<T>) -> T {
        visitor.visit_stmt(self)
    }
}
