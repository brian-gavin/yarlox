use {
    error::{LoxErrorTrait, ParseError},
    expr::Expr,
    scanner::Scanner,
    std::f64,
    stmt::Stmt,
    token::{Token, TokenType, TokenType::*},
};

/// Macro to define a left associative binary expression
/// Pass in a handle to the parser's `self`, the method name of the
/// non-terminal which has higher precedence, and coma separated `TokenType`s that
/// this expression uses
macro_rules! left_associative_binary_expr {
    ($self:ident, $higher_prec:ident, $expr_type:ident, $($matching:pat),+ ) => {{
        let mut expr = $self.$higher_prec()?;
        while match $self.peek().ttype {
            $(
                $matching => {
                    $self.advance();
                    true
                },
            )+
            _ => false,
        } {
            let op = $self.previous().clone();
            let right = Box::new($self.$higher_prec()?);
            expr = Expr::$expr_type {
                left: Box::new(expr),
                op,
                right,
            };
        }
        Ok(expr)
    }};
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    repl: bool,
    in_loop: bool,
}

impl Parser {
    pub fn new(scanner: Scanner, repl: bool) -> Result<Parser, ParseError> {
        let mut scan_had_error = false;
        let mut error: Option<ParseError> = None;
        let tokens = scanner
            .filter_map(|result| match result {
                Ok(token) => Some(token),
                Err(err) => {
                    scan_had_error = true;
                    error = Some(err);
                    None
                }
            })
            .collect();
        if scan_had_error {
            Err(error.unwrap())
        } else {
            Ok(Parser {
                tokens,
                current: 0,
                repl,
                in_loop: false,
            })
        }
    }

    pub fn parse(mut self) -> Result<Vec<Option<Stmt>>, ParseError> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            let decl = self.declaration();
            debug!("pushing decl {:?}", decl);
            stmts.push(decl);
        }
        Ok(stmts)
    }

    fn declaration(&mut self) -> Option<Stmt> {
        let res = match self.peek().ttype {
            Var => {
                self.advance();
                self.var_declaration()
            }
            _ => self.statement(),
        };
        if res.is_err() {
            res.unwrap_err().report();
            self.synchronize();
            None
        } else {
            Some(res.expect("Passed error check but failed."))
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        debug!("parsing var declaration");
        let name = self.consume(Ident, "Expect a variable name.")?.clone();

        let initializer = match self.peek().ttype {
            Equal => {
                self.advance();
                Some(Box::new(self.expression()?))
            }
            _ => None,
        };

        self.consume(Semicolon, "Expect ';' after variable declaration")?;
        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek().ttype {
            For => {
                self.advance();
                self.for_statement()
            }
            If => {
                self.advance();
                self.if_statement()
            }
            While => {
                self.advance();
                self.while_statement()
            }
            Print => {
                self.advance();
                self.print_statement()
            }
            LeftBrace => {
                self.advance();
                self.block()
            }
            Break => {
                self.advance();
                self.break_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(LeftParen, "Expected '(' after 'for'.")?;
        let initializer = match self.peek().ttype {
            Semicolon => {
                self.advance();
                None
            }
            Var => {
                self.advance();
                Some(self.var_declaration()?)
            }
            _ => {
                self.advance();
                Some(self.expression_statement()?)
            }
        };
        let condition = if self.peek().ttype != Semicolon {
            Box::new(self.expression()?)
        } else {
            Box::new(Expr::TrueLiteral)
        };
        self.consume(Semicolon, "Expected ';' after loop condition.")?;
        let increment = if self.peek().ttype != RightParen {
            Some(Box::new(self.expression()?))
        } else {
            None
        };
        self.consume(RightParen, "Expected ')' after 'for' clauses.")?;

        // build up the while loop, starting with the body
        let mut body = self.statement()?;

        // if there is an increment, add it to the end of the body
        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        // create the while loop with the condition
        body = Stmt::While {
            condition,
            body: Box::new(body),
        };

        // add the initializer
        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        /*
            {
                initializer;
                while(condition) {
                    body;
                    increment;
                }
            }
        */
        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(LeftParen, "Expected '(' after 'if'.")?;
        let condition = Box::new(self.expression()?);
        self.consume(RightParen, "Expected ')' after condition.")?;
        let then_branch = Box::new(self.statement()?);
        let else_branch = match self.peek().ttype {
            Else => {
                self.advance();
                Some(Box::new(self.statement()?))
            }
            _ => None,
        };
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.in_loop = true;
        self.consume(LeftParen, "Expected '(' after 'while'.")?;
        let condition = Box::new(self.expression()?);
        self.consume(RightParen, "Expected ')' after condition.")?;
        let body = Box::new(self.statement()?);
        self.in_loop = false;
        Ok(Stmt::While { condition, body })
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        debug!("parsing a print statement");
        let expr = Box::new(self.expression()?);
        self.consume(Semicolon, "Expected ';' after value.")?;
        Ok(Stmt::Print(expr))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = Box::new(self.expression()?);
        if self.repl && self.is_at_end() {
            Ok(Stmt::Print(expr))
        } else {
            self.consume(Semicolon, "Expected ';' after expression.")?;
            Ok(Stmt::Expression(expr))
        }
    }

    fn block(&mut self) -> Result<Stmt, ParseError> {
        let mut stmts = Vec::new();
        while self.peek().ttype != RightBrace && !self.is_at_end() {
            let next = self.declaration();
            if let Some(next) = next {
                stmts.push(next);
            }
        }
        self.consume(RightBrace, "Expect '}' after block.")?;
        Ok(Stmt::Block(stmts))
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        use expr::Expr::{Assign, Variable};

        let expr = self.or()?;
        debug!("assignment parsed: {:?}", expr);
        match self.peek().ttype {
            Equal => {
                self.advance();
                let equals = self.previous().clone();
                let value = Box::new(self.assignment()?);

                let assign = match expr {
                    Variable { name } => Assign { name, value },
                    _ => {
                        self.error(&equals, "Invalid assignment target").report();
                        expr
                    }
                };
                Ok(assign)
            }
            _ => Ok(expr),
        }
    }

    fn break_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.in_loop {
            self.consume(Semicolon, "Expected ';' after 'break'.")?;
            Ok(Stmt::Break)
        } else {
            Err(self.error(self.previous(), "Must be in a loop to 'break'."))
        }
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, and, Logical, Or)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, equality, Logical, And)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, comparison, Binary, BangEqual, EqualEqual)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(
            self,
            addition,
            Binary,
            Greater,
            GreaterEqual,
            Less,
            LessEqual
        )
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, multiplication, Binary, Plus, Minus)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, unary, Binary, Star, Slash)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        match self.peek().ttype {
            Bang | Minus => {
                self.advance();
                let op = self.previous().clone();
                let right = Box::new(self.unary()?);
                Ok(Expr::Unary { op, right })
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        Ok(match self.peek().ttype {
            False => {
                self.advance();
                Expr::FalseLiteral
            }
            True => {
                self.advance();
                Expr::TrueLiteral
            }
            Nil => {
                self.advance();
                Expr::NilLiteral
            }
            Number => {
                self.advance();
                let number = match self.previous().literal.parse::<f64>() {
                    Ok(n) => n,
                    Err(e) => return Err(self.error(self.previous(), e.to_string().as_str())),
                };
                Expr::NumberLiteral(number)
            }
            StringLit => {
                self.advance();
                Expr::StringLiteral(self.previous().literal.clone())
            }
            LeftParen => {
                self.advance();
                let expr = self.expression()?;
                let _ = self.consume(RightParen, "Expect ')' after expression.")?;
                Expr::Grouping(Box::new(expr))
            }
            Ident => {
                self.advance();
                Expr::Variable {
                    name: self.previous().clone(),
                }
            }
            _ => {
                let peeked = self.peek();
                return Err(self.error(peeked, "Expected an expression."));
            }
        })
    }

    fn consume(&mut self, expected_type: TokenType, error_msg: &str) -> Result<&Token, ParseError> {
        if self.peek().ttype == expected_type {
            Ok(self.advance())
        } else {
            let peeked = self.peek();
            Err(self.error(peeked, error_msg))
        }
    }

    fn error(&self, token: &Token, msg: &str) -> ParseError {
        if token.ttype == EOF {
            ParseError::new(token.line, "at end".to_string(), msg.to_string())
        } else {
            let mut e_type = String::new();
            e_type.push_str("at '");
            e_type.push_str(token.lexeme.as_str());
            e_type.push('\'');
            ParseError::new(token.line, e_type, msg.to_string())
        }
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if self.previous().ttype == Semicolon {
                return;
            }
            match self.peek().ttype {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => self.advance(),
            };
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        let rv = self.previous();
        debug!("advance to: {:?}", *rv);
        rv
        // self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().ttype == EOF
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
