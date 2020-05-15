use {expr::Expr, token::Token};

#[derive(Debug, Clone)]
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
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Break,
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
        is_class_method: bool,
    },
    Return {
        keyword: Token,
        expr: Option<Box<Expr>>,
    },
    Class {
        name: Token,
        methods: Vec<Stmt>,
    },
    GetterMethod {
        name: Token,
        body: Vec<Stmt>,
        is_class_method: bool,
    },
}
