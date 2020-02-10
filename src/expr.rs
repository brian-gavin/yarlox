use {ast_printer::Printer, std::fmt, token::Token};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Logical {
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
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ast = Printer.print(self);
        f.write_str(&ast)?;
        Ok(())
    }
}
