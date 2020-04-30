use {ast_printer::Printer, std::fmt, token::Token};

#[derive(Debug, Clone)]
pub struct Expr {
    pub distance: Option<usize>,
    pub kind: ExprKind,
}

impl Expr {
    pub const fn of(kind: ExprKind) -> Expr {
        Expr {
            distance: None,
            kind,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
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
