pub mod expr {
    use expr::Expr;

    pub trait Visitor<T> {
        fn visit_expr(&self, expr: &Expr) -> T;
    }

    pub trait Visitable<T> {
        fn accept(&self, visitor: &impl Visitor<T>) -> T;
    }
}

pub mod stmt {
    use stmt::Stmt;

    pub trait Visitor<T> {
        fn visit_stmt(&mut self, expr: &Stmt) -> T;
    }

    pub trait Visitable<T> {
        fn accept(&self, visitor: &mut impl Visitor<T>) -> T;
    }
}
