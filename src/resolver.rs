use {
    error::ParseError,
    expr::{Expr, ExprKind::*},
    parser::Parser,
    std::collections::HashMap,
    stmt::Stmt,
    token::Token,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VariableState {
    Uninitialized,
    Initialized,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, VariableState>>,
    current_function: FunctionType,
}

type ResolveReturn = Result<(), ParseError>;

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            scopes: vec![],
            current_function: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &mut [Stmt]) -> ResolveReturn {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> ResolveReturn {
        match stmt {
            Stmt::Block(stmts) => {
                debug!("resolving block stmt...");
                self.begin_scope();
                self.resolve(stmts)?;
                self.end_scope();
            }
            Stmt::Var { name, initializer } => {
                debug!("resolving var stmt...");
                self.declare(&name)?;
                if let Some(initializer) = initializer {
                    debug!("resolving initializer...");
                    self.resolve_expr(initializer)?;
                    debug!("initializer finished");
                }
                self.define(&name);
                debug!("var stmt finished")
            }
            Stmt::Function { name, .. } => {
                self.declare(name)?;
                self.define(name);
                self.resolve_function(stmt, FunctionType::Function)?;
            }
            Stmt::Return { keyword, expr } => {
                if self.current_function == FunctionType::None {
                    return Err(Parser::error(keyword, "Cannot return from top-level code."));
                }
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
            }
            Stmt::Print(expr) | Stmt::Expression(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
            }
            Stmt::Break => (),
            Stmt::Class { name, .. } => {
                self.declare(name)?;
                self.define(name);
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> ResolveReturn {
        match expr.kind {
            Variable { ref name } => {
                let variable_in_scope_and_uninit = self
                    .scopes
                    .last()
                    .and_then(|scope| scope.get(&name.lexeme))
                    .map(|value| *value == VariableState::Uninitialized)
                    .unwrap_or(false);
                if variable_in_scope_and_uninit {
                    return Err(Parser::error(
                        name,
                        "Cannot read local variable in its own initializer.",
                    ));
                } else {
                    expr.distance = self.resolve_local(name);
                }
            }
            Assign {
                ref mut value,
                ref name,
            } => {
                self.resolve_expr(value)?;
                expr.distance = self.resolve_local(name);
            }
            Logical {
                ref mut left,
                ref mut right,
                ..
            }
            | Binary {
                ref mut left,
                ref mut right,
                ..
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Call {
                ref mut callee,
                ref mut arguments,
                ..
            } => {
                self.resolve_expr(callee)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
            }
            Grouping(ref mut expr)
            | Unary {
                right: ref mut expr,
                ..
            } => {
                self.resolve_expr(expr)?;
            }
            TrueLiteral | FalseLiteral | StringLiteral(_) | NumberLiteral(_) | NilLiteral => (),
        }
        Ok(())
    }

    fn resolve_function(&mut self, stmt: &mut Stmt, ftype: FunctionType) -> ResolveReturn {
        match stmt {
            Stmt::Function {
                params,
                ref mut body,
                ..
            } => {
                let enclosing_fn = self.current_function;
                self.current_function = ftype;
                self.begin_scope();
                for param in params {
                    self.declare(param)?;
                    self.define(param);
                }
                self.resolve(body)?;
                self.end_scope();
                self.current_function = enclosing_fn;
            }
            _ => panic!("Resolving statement that isn't a function."),
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> ResolveReturn {
        debug!("in declare: scope len {:?}", self.scopes.len());
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(Parser::error(
                    name,
                    "Variable with this name already declared in this scope.",
                ));
            }
            scope.insert(name.lexeme.clone(), VariableState::Uninitialized);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        debug!(
            "define({}) in scope[{}]",
            name.lexeme,
            self.scopes.len() - 1
        );
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), VariableState::Initialized);
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Option<usize> {
        debug!(
            "resolving local: {} scopes len: {}",
            name.lexeme,
            self.scopes.len()
        );
        if self.scopes.len() == 0 {
            return None;
        }
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                let distance = self.scopes.len() - 1 - i;
                debug!("resolution found: {} distance: {}", name.lexeme, distance);
                return Some(distance);
            }
        }
        debug!("resolution assumes global");
        None
    }
}
