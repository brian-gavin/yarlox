use itertools::{multipeek, MultiPeek};
use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    Str,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize) -> Token {
        Token { kind, lexeme, line }
    }
}

fn identifier_kind(lexeme: &str) -> TokenKind {
    match lexeme {
        "and" => TokenKind::And,
        "class" => TokenKind::Class,
        "else" => TokenKind::Else,
        "false" => TokenKind::False,
        "for" => TokenKind::For,
        "fun" => TokenKind::Fun,
        "if" => TokenKind::If,
        "nil" => TokenKind::Nil,
        "or" => TokenKind::Or,
        "print" => TokenKind::Print,
        "return" => TokenKind::Return,
        "super" => TokenKind::Super,
        "this" => TokenKind::This,
        "true" => TokenKind::True,
        "var" => TokenKind::Var,
        "while" => TokenKind::While,
        _ => TokenKind::Identifier,
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: MultiPeek<Chars<'a>>,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &str) -> Scanner {
        let source = multipeek(source.chars());
        Scanner { source, line: 1 }
    }

    fn make_token<S: Into<String>>(&self, kind: TokenKind, lexeme: S) -> Token {
        Token::new(kind, lexeme.into(), self.line)
    }

    fn is_next(&mut self, expected: char) -> Option<bool> {
        if *self.source.peek()? == expected {
            self.source.next()?;
            Some(true)
        } else {
            self.source.reset_peek();
            Some(false)
        }
    }

    fn skip_whitespace(&mut self) -> Option<()> {
        loop {
            match self.source.peek()? {
                '\n' => {
                    self.line += 1;
                    self.source.next()?;
                }
                c if c.is_whitespace() => {
                    self.source.next()?;
                }
                '/' => {
                    if *self.source.peek()? == '/' {
                        while self.source.peek() != Some(&'\n') {
                            self.source.next()?;
                        }
                        self.source.reset_peek();
                    }
                }
                _ => return Some(()),
            }
        }
    }

    fn string(&mut self) -> Option<Token> {
        let mut lexeme = String::from("\"");
        while match self.source.peek() {
            Some(&'"') | None => false,
            _ => true,
        } {
            lexeme.push(self.source.next().expect("Uh oh"));
        }
        if self.source.peek().is_none() {
            self.source.reset_peek();
            Some(self.make_token(TokenKind::Error, "Unterminated string."))
        } else {
            lexeme.push(self.source.next().expect("Uh oh"));
            Some(self.make_token(TokenKind::Str, lexeme))
        }
    }

    fn number(&mut self, c: char) -> Token {
        let mut lexeme = c.to_string();
        while let Some(c) = self.source.peek() {
            if c.is_digit(10) {
                lexeme.push(self.source.next().unwrap());
            } else {
                break;
            }
        }
        self.source.reset_peek();
        if self.source.peek() == Some(&'.') {
            lexeme.push(self.source.next().unwrap());
            while let Some(c) = self.source.peek() {
                if c.is_digit(10) {
                    lexeme.push(self.source.next().unwrap());
                } else {
                    break;
                }
            }
        }
        self.source.reset_peek();
        self.make_token(TokenKind::Number, lexeme)
    }

    fn identifier(&mut self, c: char) -> Token {
        let mut lexeme = c.to_string();
        while let Some(c) = self.source.peek() {
            if c.is_alphanumeric() || *c == '_' {
                lexeme.push(self.source.next().unwrap());
            } else {
                break;
            }
        }
        self.source.reset_peek();
        let kind = identifier_kind(&lexeme);
        self.make_token(kind, lexeme)
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace()?;
        let tok = match self.source.next()? {
            '(' => self.make_token(TokenKind::LeftParen, "("),
            ')' => self.make_token(TokenKind::RightParen, ")"),
            '{' => self.make_token(TokenKind::LeftBrace, "{"),
            '}' => self.make_token(TokenKind::RightBrace, "}"),
            ';' => self.make_token(TokenKind::Semicolon, ";"),
            ',' => self.make_token(TokenKind::Comma, ","),
            '.' => self.make_token(TokenKind::Dot, "."),
            '-' => self.make_token(TokenKind::Minus, "-"),
            '+' => self.make_token(TokenKind::Plus, "+"),
            '/' => self.make_token(TokenKind::Slash, "/"),
            '*' => self.make_token(TokenKind::Star, "*"),

            '!' if self.is_next('=')? => self.make_token(TokenKind::BangEqual, "!="),
            '!' => self.make_token(TokenKind::Bang, "!"),

            '=' if self.is_next('=')? => self.make_token(TokenKind::EqualEqual, "=="),
            '=' => self.make_token(TokenKind::Equal, "="),

            '<' if self.is_next('=')? => self.make_token(TokenKind::LessEqual, "<="),
            '<' => self.make_token(TokenKind::Less, "<"),

            '>' if self.is_next('=')? => self.make_token(TokenKind::GreaterEqual, ">="),
            '>' => self.make_token(TokenKind::Greater, ">"),

            '"' => self.string()?,

            c if c.is_digit(10) => self.number(c),
            c if c.is_alphabetic() || c == '_' => self.identifier(c),

            c => self.make_token(TokenKind::Error, format!("Unexpected character {}.", c)),
        };
        Some(tok)
    }
}
