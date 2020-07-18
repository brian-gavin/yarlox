use {
    environment::Environment,
    error::RuntimeError,
    expr::{Expr, ExprKind::*},
    std::{
        cell::{RefCell, RefMut},
        collections::HashMap,
        rc::Rc,
    },
    stmt::{Stmt, Stmt::*},
    token::Token,
    types::{LoxClass, LoxFunction, LoxInstance, LoxType},
};

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
}

#[derive(Debug, PartialEq)]
pub enum ExecuteReturn {
    Void,
    Break,
    Return(LoxType),
}

macro_rules! execute_handle_return_or_break {
    ($self:ident, $stmt:ident) => {
        let rv = $self.execute($stmt)?;
        match rv {
            ExecuteReturn::Break | ExecuteReturn::Return(_) => {
                return Ok(rv);
            }
            _ => (),
        }
    };
}

fn define_globals(mut globals: RefMut<'_, Environment>) {
    globals.define(String::from("clock"), LoxType::BuiltinFnClock.into());
    globals.define(String::from("PI"), LoxType::Number(std::f64::consts::PI))
}
impl Interpreter {
    pub fn new() -> Interpreter {
        let globals = Rc::new(RefCell::new(Environment::new()));
        define_globals(globals.borrow_mut());
        let environment = globals.clone();
        Interpreter {
            globals,
            environment,
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts.iter() {
            self.execute(stmt)?;
        }
        Ok(())
    }

    pub fn execute_block(
        &mut self,
        stmts: &[Stmt],
        environment: Environment,
    ) -> Result<ExecuteReturn, RuntimeError> {
        let old_environment = self.environment.clone();
        self.environment = Rc::new(RefCell::new(environment));
        let mut rv = ExecuteReturn::Void;
        for stmt in stmts.iter() {
            rv = self.execute(stmt)?;
            match rv {
                ExecuteReturn::Return(_) | ExecuteReturn::Break => {
                    break;
                }
                _ => (),
            };
        }
        self.environment = old_environment;
        Ok(rv)
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<ExecuteReturn, RuntimeError> {
        match stmt {
            Print(expr) => {
                println!("{}", self.evaluate(expr)?.stringify());
            }
            Expression(expr) => {
                self.evaluate(expr)?;
            }
            Var { name, initializer } => {
                let val = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => LoxType::Nil,
                };
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), val);
            }
            Block(stmts) => {
                let environment = Environment::from(self.environment.clone());
                return self.execute_block(stmts, environment);
            }
            If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.evaluate(condition)?.is_truthy() {
                    execute_handle_return_or_break!(self, then_branch);
                } else if let Some(else_branch) = else_branch {
                    execute_handle_return_or_break!(self, else_branch);
                }
            }
            While { condition, body } => {
                while self.evaluate(condition)?.is_truthy() {
                    let rv = self.execute(body)?;
                    match rv {
                        ExecuteReturn::Break => break,
                        ExecuteReturn::Return(_) => return Ok(rv),
                        _ => (),
                    }
                }
            }
            Break => return Ok(ExecuteReturn::Break),
            Function { name, .. } => {
                let func = LoxFunction::new(stmt.clone(), self.environment.clone(), false);
                let func = LoxType::LoxFunction(Rc::new(func));
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), func);
            }
            Return { expr, .. } => {
                return Ok(ExecuteReturn::Return(match expr {
                    Some(expr) => self.evaluate(expr)?,
                    None => LoxType::Nil.into(),
                }))
            }
            Class { name, super_class, methods: method_decls } => {
                let super_class = if let Some(super_class) = super_class {
                    let sc_type = self.evaluate(&super_class)?;
                    match sc_type {
                        LoxType::LoxClass(super_class) => Some(super_class.clone()),
                        _ => {
                            let super_class_name = if let Variable{ref name} = super_class.kind {
                                name
                            } else {
                                unreachable!()
                            };
                            return Err(Self::error(super_class_name.clone(), "Superclass must be a class."));
                        }
                    }
                } else {
                    None
                };
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), LoxType::Nil.into());
                let mut methods = HashMap::new();
                let mut class_methods = HashMap::new();
                for method in method_decls {
                    match method {
                        Stmt::Function { name, is_class_method, .. } | Stmt::GetterMethod { name, is_class_method, .. } => {
                            let function = Rc::new(LoxFunction::new(
                                method.clone(),
                                self.environment.clone(),
                                name.lexeme == "init",
                            ));
                            if *is_class_method {
                                class_methods.insert(name.lexeme.clone(), function);
                            } else {
                                methods.insert(name.lexeme.clone(), function);
                            }
                        }
                        _ => panic!("Non-function in list of class methods"),
                    }
                }
                let class = LoxType::LoxClass(Rc::new(LoxClass::new(name.lexeme.clone(), super_class, methods, class_methods)));
                self.environment.borrow_mut().assign(name, class.into())?;
            }
            GetterMethod{..} => panic!("GetterMethod should not be executed directly, it should be done in Class execution."),
        }
        Ok(ExecuteReturn::Void)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxType, RuntimeError> {
        use self::LoxType::{Boolean, LoxString, Nil, Number};
        let res = match &expr.kind {
            StringLiteral(s) => LoxString(s.clone()),
            NumberLiteral(n) => Number(*n),
            NilLiteral => Nil,
            TrueLiteral => Boolean(true),
            FalseLiteral => Boolean(false),
            Grouping(grouped) => self.evaluate(grouped)?,
            Unary { op, right } => self.evaluate_unary(op, right)?,
            Logical { left, op, right } => self.evaluate_logical(left, op, right)?,
            Binary { left, op, right } => self.evaluate_binary(left, op, right)?,
            Variable { name } | This(name) => self.look_up_variable(name, expr)?,
            Assign { name, value } => self.assign_variable(expr, name, value)?,
            Call {
                arguments,
                callee,
                paren,
            } => {
                let callee = self.evaluate(callee)?;
                let mut evaluated_arguments = vec![];
                for arg in arguments.iter() {
                    evaluated_arguments.push(self.evaluate(arg)?);
                }
                if evaluated_arguments.len() != callee.arity() {
                    return Err(Self::error(
                        paren.clone(),
                        format!(
                            "Expected {} arguments but got {}.",
                            callee.arity(),
                            evaluated_arguments.len()
                        )
                        .as_str(),
                    ));
                }
                callee.call(paren, self, &evaluated_arguments)?
            }
            Get { object, name } => {
                let object = self.evaluate(object)?;
                LoxInstance::get(&object, name, self)?
            }
            Set {
                object,
                name,
                value,
            } => {
                let object = self.evaluate(object)?;
                let rv = if let LoxType::LoxInstance(instance) = object {
                    let value = self.evaluate(value)?;
                    instance.borrow_mut().set(name, value.clone());
                    value
                } else {
                    return Err(Self::error(name.clone(), "Only instances have properties."));
                };
                rv
            }
        };
        Ok(res)
    }

    fn look_up_variable(&mut self, name: &Token, expr: &Expr) -> Result<LoxType, RuntimeError> {
        if let Some(distance) = expr.distance {
            self.environment.borrow_mut().get_at(distance, name)
        } else {
            self.globals.borrow_mut().get(name)
        }
    }

    fn assign_variable(
        &mut self,
        expr: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<LoxType, RuntimeError> {
        let value = self.evaluate(value)?;
        if let Some(distance) = expr.distance {
            self.environment
                .borrow_mut()
                .assign_at(distance, name, value.clone())?;
        } else {
            self.globals.borrow_mut().assign(name, value.clone())?;
        }
        Ok(value)
    }

    fn evaluate_unary(&mut self, op: &Token, right: &Expr) -> Result<LoxType, RuntimeError> {
        use {
            token::TokenType::{Bang, Minus},
            types::LoxType::{Boolean, Number},
        };

        let right = self.evaluate(right)?;
        let res = match (op.ttype, right) {
            (Minus, Number(n)) => Number(-n),
            (Minus, _) => return Err(self.number_error(op.clone())),
            (Bang, right @ _) => Boolean(!right.is_truthy()),
            _ => panic!("Unary operator that is neither Bang nor Minus"),
        };
        Ok(res.into())
    }

    fn evaluate_logical(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<LoxType, RuntimeError> {
        use token::TokenType::Or;
        let left = self.evaluate(left)?;

        if op.ttype == Or {
            if left.is_truthy() {
                return Ok(left);
            }
        } else {
            if !left.is_truthy() {
                return Ok(left);
            }
        }
        self.evaluate(right)
    }

    fn evaluate_binary(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<LoxType, RuntimeError> {
        use {
            token::TokenType::{
                BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash,
                Star,
            },
            types::LoxType::{Boolean, LoxString, Number},
        };

        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        let res = match (left, op.ttype, right) {
            (Number(n1), Star, Number(n2)) => Number(n1 * n2),
            (Number(n1), Slash, Number(n2)) => {
                if n2 != 0.0 {
                    Number(n1 / n2)
                } else {
                    return Err(Self::error(op.clone(), "Divide by zero"));
                }
            }
            (Number(n1), Minus, Number(n2)) => Number(n1 - n2),
            (Number(n1), Plus, Number(n2)) => Number(n1 + n2),
            (LoxString(s), Plus, Number(n)) => LoxString(s.clone() + &n.to_string()),
            (Number(n), Plus, LoxString(s)) => LoxString(n.to_string() + &s),
            (LoxString(s1), Plus, LoxString(s2)) => LoxString(s1.clone() + &s2),
            (Number(n1), Greater, Number(n2)) => Boolean(n1 > n2),
            (Number(n1), GreaterEqual, Number(n2)) => Boolean(n1 >= n2),
            (Number(n1), Less, Number(n2)) => Boolean(n1 < n2),
            (Number(n1), LessEqual, Number(n2)) => Boolean(n1 <= n2),
            (ref o1 @ _, EqualEqual, ref o2 @ _) => Boolean(o1 == o2),
            (ref o1 @ _, BangEqual, ref o2 @ _) => Boolean(o1 != o2),
            (_, Star, _)
            | (_, Slash, _)
            | (_, Minus, _)
            | (_, Greater, _)
            | (_, GreaterEqual, _)
            | (_, Less, _)
            | (_, LessEqual, _) => return Err(self.number_error(op.clone())),
            (_, Plus, _) => {
                return Err(Self::error(
                    op.clone(),
                    "Operators must be numbers or strings",
                ));
            }
            (_, _, _) => return Err(Self::error(op.clone(), "Unexpected binary expression")),
        };
        Ok(res.into())
    }

    fn number_error(&self, token: Token) -> RuntimeError {
        Self::error(token, "Operands must be numbers.")
    }

    pub fn error(token: Token, msg: &str) -> RuntimeError {
        let msg = msg.to_string();
        RuntimeError { token, msg }
    }
}
