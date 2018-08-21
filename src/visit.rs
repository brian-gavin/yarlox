use expr::Expr;

pub trait Visitor<T> {
    fn visitExpr(&self, expr: &Expr) -> T;
}

pub trait Visitable<T> {
    fn accept(&self, visitor: &impl Visitor<T>) -> T;
}
