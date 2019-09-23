use {expr::Expr, token::Token};

#[derive(Debug)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var {
        name: Token,
        initializer: Option<Box<Expr>>,
    },
    Block(Vec<Stmt>),
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
}
