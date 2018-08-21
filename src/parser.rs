use {
    expr::Expr,
    report_exit,
    scanner::Scanner,
    token::{Token, TokenType, TokenType::*},
    ParseError,
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        Parser {
            tokens: scanner.collect(),
            current: 0,
        }
    }

    pub fn parse(mut self) -> Expr {
        self.expression().unwrap()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while match self.peek().ttype {
            BangEqual | EqualEqual => {
                self.advance();
                true
            }
            _ => false,
        } {
            let op = self.previous().clone();
            let right = Box::new(self.comparison()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.addition()?;
        while match self.peek().ttype {
            Greater | GreaterEqual | Less | LessEqual => {
                self.advance();
                true
            }
            _ => false,
        } {
            let op = self.previous().clone();
            let right = Box::new(self.addition()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            };
        }
        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.multiplication()?;
        while match self.peek().ttype {
            Plus | Minus => {
                self.advance();
                true
            }
            _ => false,
        } {
            let op = self.previous().clone();
            let right = Box::new(self.multiplication()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            };
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while match self.peek().ttype {
            Star | Slash => {
                self.advance();
                true
            }
            _ => false,
        } {
            let op = self.previous().clone();
            let right = Box::new(self.unary()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        Ok(expr)
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
            Number | StringLit => {
                self.advance();
                Expr::StringLiteral(self.previous().literal.clone())
            }
            LeftParen => {
                self.advance();
                let expr = self.expression()?;
                let _ = self.consume(RightParen, "Expect ')' after expression.")?;
                Expr::Grouping(Box::new(expr))
            }
            _ => {
                let peeked = self.peek();
                report_exit(self.error(peeked, "Expected an expression."))
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
        eprintln!("{:?}", *rv);
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
