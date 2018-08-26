use {
    expr::Expr,
    std::{f64, str::FromStr},
    scanner::Scanner,
    token::{Token, TokenType, TokenType::*},
    ParseError,
};

/// Macro to define a left associative binary expression
/// Pass in a handle to the parser's `self`, the method name of the
/// non-terminal which has higher precedence, and coma separated `TokenType`s that
/// this expression uses
macro_rules! left_associative_binary_expr {
    ($self:ident, $higher_prec:ident, $($matching:pat),+ ) => {{
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
            expr = Expr::Binary {
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
}

impl Parser {
    pub fn new(scanner: Scanner) -> Result<Parser, ParseError> {
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
            Ok(Parser { tokens, current: 0 })
        }
    }

    pub fn parse(mut self) -> Expr {
        self.expression().unwrap()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, comparison, BangEqual, EqualEqual)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, addition, Greater, GreaterEqual, Less, LessEqual)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, multiplication, Plus, Minus)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        left_associative_binary_expr!(self, unary, Star, Slash)
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
                let number = match f64::from_str(&self.previous().literal) {
                    Ok(n) => n,
                    Err(e) => return Err(self.error(self.previous(), e.to_string().as_str()))
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
            _ => {
                let peeked = self.peek();
                return Err(self.error(peeked, "Expected an expression."))
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
        eprintln!("advance to: {:?}", *rv);
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
