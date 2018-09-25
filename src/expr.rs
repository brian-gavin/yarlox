use {
    ast_printer::Printer,
    token::Token,
    visit::expr::{Visitable, Visitor},
};

#[derive(Debug)]
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
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
}

impl<T> Visitable<T> for Expr {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> T {
        visitor.visit_expr(self)
    }
}

impl Expr {
    pub fn print(self) {
        println!("{}", Printer.print(&self))
    }
}
