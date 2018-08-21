use {
    ast_printer::Printer,
    token::Token,
    visit::{Visitable, Visitor},
};

pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    StringLiteral(String),
    NumberLiteral(f64),
    NilLiteral,
    FalseLiteral,
    TrueLiteral,
    Grouping(Box<Expr>),
    Unary {
        op: Token,
        right: Box<Expr>,
    },
}

impl<T> Visitable<T> for Expr {
    fn accept(&self, visitor: &impl Visitor<T>) -> T {
        visitor.visitExpr(self)
    }
}

impl Expr {
    pub fn print(self) {
        println!("{}", Printer.print(&self))
    }
}
