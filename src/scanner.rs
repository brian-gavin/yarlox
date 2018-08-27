use {
    parser::Parser,
    std::collections::{HashMap, VecDeque},
    std::str::Chars,
    token::TokenType::*,
    token::{Token, TokenType},
    ParseError,
};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = [
        ("and", And),
        ("class", Class),
        ("else", Else),
        ("false", False),
        ("for", For),
        ("fun", Fun),
        ("if", If),
        ("nil", Nil),
        ("or", Or),
        ("print", Print),
        ("return", Return),
        ("super", Super),
        ("this", This),
        ("true", True),
        ("var", Var),
        ("while", While),
    ].iter()
        .cloned()
        .collect();
}

const LOOKAHEAD: usize = 2;

pub struct Scanner<'a> {
    source: Chars<'a>,
    peeks: VecDeque<char>,
    line: u32,
    eof: bool,
    had_error: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        Scanner {
            source: source.chars(),
            peeks: VecDeque::with_capacity(LOOKAHEAD),
            line: 1,
            eof: false,
            had_error: false,
        }
    }

    pub fn parser(self) -> Result<Parser, ParseError> {
        Parser::new(self)
    }

    fn basic_token(&self, ttype: TokenType, lexeme: &'static str) -> Token {
        Token::build()
            .ttype(ttype)
            .lexeme_str(lexeme)
            .literal_str(lexeme)
            .line(self.line)
            .finalize()
    }

    fn next_token(&mut self, c: Option<char>) -> Result<Token, ParseError> {
        if c.is_none() {
            self.eof = true;
            let eof_token = Token::build().ttype(EOF).line(self.line).finalize();
            return Ok(eof_token);
        }
        let c = c.unwrap();
        let skip = match c {
            ' ' | '\r' | '\t' => true,
            '\n' => {
                self.line += 1;
                true
            }
            _ => false,
        };
        if skip {
            let advance = self.advance();
            return Ok(self.next_token(advance)?);
        }

        let next_token = match c {
            '(' => self.basic_token(LeftParen, "("),
            ')' => self.basic_token(RightParen, ")"),
            '{' => self.basic_token(LeftBrace, "{"),
            '}' => self.basic_token(RightBrace, "}"),
            ',' => self.basic_token(Comma, ","),
            '.' => self.basic_token(Dot, "."),
            '-' => self.basic_token(Minus, "-"),
            '+' => self.basic_token(Plus, "+"),
            ';' => self.basic_token(Semicolon, ";"),
            '*' => self.basic_token(Star, "*"),
            '!' | '=' | '<' | '>' => self.comparison_bang_or_equal(c)?,
            '/' => {
                if let Some(token) = self.comment_or_slash() {
                    token
                } else {
                    let advance = self.advance();
                    self.next_token(advance)?
                }
            }
            '"' => self.string()?,
            '0'...'9' => self.number(c)?,
            _ => self.ident(c)?,
        };

        Ok(next_token)
    }

    fn advance(&mut self) -> Option<char> {
        let peeked = self.peeks.pop_front();
        if peeked.is_some() {
            peeked
        } else {
            let next = self.source.next();
            next
        }
    }

    fn peek(&mut self, amt: usize) -> Option<char> {
        assert!(1 <= amt && amt <= LOOKAHEAD);
        assert!(self.peeks.len() <= LOOKAHEAD);
        if let Some(c) = self.peeks.get(amt - 1) {
            return Some(*c);
        }
        for _ in self.peeks.len()..amt {
            if let Some(c) = self.source.next() {
                self.peeks.push_back(c);
            }
        }
        self.peek(amt)
    }

    fn comment_or_slash(&mut self) -> Option<Token> {
        match self.peek(1) {
            Some('/') => {
                // ignore comments after parsing
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
                self.line += 1;
                None
            }
            _ => {
                Some(self.basic_token(Slash, "/"))
            }
        }
    }

    fn comparison_bang_or_equal(&mut self, c: char) -> Result<Token, ParseError> {
        match (c, self.peek(1)) {
            ('!', Some('=')) => {
                self.advance();
                Ok(self.basic_token(BangEqual, "!="))
            }
            ('=', Some('=')) => {
                self.advance();
                Ok(self.basic_token(EqualEqual, "=="))
            }
            ('<', Some('=')) => {
                self.advance();
                Ok(self.basic_token(LessEqual, "<="))
            }
            ('>', Some('=')) => {
                self.advance();
                Ok(self.basic_token(GreaterEqual, ">="))
            }
            ('!', _) => Ok(self.basic_token(Bang, "!")),
            ('=', _) => Ok(self.basic_token(Equal, "=")),
            ('<', _) => Ok(self.basic_token(Less, "<")),
            ('>', _) => Ok(self.basic_token(Greater, ">")),
            (_, _) => Err(self.unexpected_character(c)),
        }
    }

    fn string(&mut self) -> Result<Token, ParseError> {
        let mut lexeme = String::new();
        lexeme.push('"');
        while let Some(c) = self.advance() {
            match c {
                '"' => {
                    lexeme.push('"');
                    break;
                }
                '\n' => {
                    lexeme.push('\n');
                    self.line += 1;
                }
                _ => lexeme.push(c),
            }
        }
        let literal = lexeme[1..lexeme.len() - 1].to_string();
        // terminated means that we didn't hit EOF
        let terminated = self.peek(1).is_some();
        if terminated {
            let tok = Token::build()
                .ttype(StringLit)
                .lexeme(lexeme)
                .line(self.line)
                .literal(literal)
                .finalize();
            Ok(tok)
        } else {
            Err(self.error("".to_string(), "Unterminated string".to_string()))
        }
    }

    fn number(&mut self, first: char) -> Result<Token, ParseError> {
        let mut lexeme = String::new();
        lexeme.push(first);
        while let Some(c) = self.peek(1) {
            match c {
                '0'...'9' => {
                    lexeme.push(c);
                    self.advance();
                }
                '.' => {
                    let peeked = self.peek(2);
                    match peeked {
                        Some(c2 @ '0'...'9') => {
                            lexeme.push(c);
                            lexeme.push(c2);
                            self.advance();
                        }
                        Some(_) | None => {
                            return Err(self.error(
                                "Invalid number".to_string(),
                                "Expected digit after '.'".to_string(),
                            ))
                        }
                    }
                }
                _ => break,
            }
        }
        let literal = lexeme.clone();
        Ok(Token::build()
            .ttype(Number)
            .lexeme(lexeme)
            .literal(literal)
            .line(self.line)
            .finalize())
    }

    fn ident(&mut self, first: char) -> Result<Token, ParseError> {
        if !first.is_alphanumeric() && first != '_' {
            return Err(self.unexpected_character(first));
        }
        let mut lexeme = String::from(first.to_string());

        while let Some(c) = self.peek(1) {
            if c.is_alphanumeric() || c == '_' {
                lexeme.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let mut token_builder = Token::build()
            .literal(lexeme.clone())
            .lexeme(lexeme.clone())
            .line(self.line);
        if let Some(res_token) = KEYWORDS.get(lexeme.as_str()) {
            token_builder = token_builder.ttype(res_token.clone());
        } else {
            token_builder = token_builder.ttype(Ident);
        }
        Ok(token_builder.finalize())
    }

    fn error(&self, e_type: String, msg: String) -> ParseError {
        ParseError {
            line: self.line,
            e_type,
            msg,
        }
    }

    fn unexpected_character(&self, c: char) -> ParseError {
        self.error("unexpected character".to_string(), c.to_string())
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        if self.eof {
            None
        } else if self.had_error {
            None
        } else {
            let advance = self.advance();
            let next_token = self.next_token(advance);
            self.had_error = next_token.is_err();
            Some(next_token)
        }
    }
}
