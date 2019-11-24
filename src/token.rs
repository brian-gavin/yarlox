#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TokenType {
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
    Ident,
    StringLit,
    Number,

    // Keywords.
    And,
    Break,
    Class,
    Else,
    False,
    Fun,
    For,
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

    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub literal: String,
    pub line: u32,
}

impl Token {
    pub fn build() -> TokenBuilder {
        TokenBuilder::new()
    }
}

pub struct TokenBuilder {
    ttype: TokenType,
    lexeme: String,
    literal: String,
    line: u32,
}

impl TokenBuilder {
    pub fn new() -> TokenBuilder {
        TokenBuilder {
            ttype: TokenType::EOF,
            lexeme: String::new(),
            literal: String::new(),
            line: 1,
        }
    }

    pub fn ttype(mut self, ttype: TokenType) -> TokenBuilder {
        self.ttype = ttype;
        self
    }

    pub fn lexeme(mut self, lexeme: String) -> TokenBuilder {
        self.lexeme = lexeme;
        self
    }

    pub fn lexeme_str(mut self, lexeme: &'static str) -> TokenBuilder {
        self.lexeme = lexeme.to_string();
        self
    }

    pub fn literal(mut self, literal: String) -> TokenBuilder {
        self.literal = literal;
        self
    }

    pub fn literal_str(mut self, literal: &'static str) -> TokenBuilder {
        self.literal = literal.to_string();
        self
    }

    pub fn line(mut self, line: u32) -> TokenBuilder {
        self.line = line;
        self
    }

    pub fn finalize(self) -> Token {
        Token {
            ttype: self.ttype,
            lexeme: self.lexeme,
            literal: self.literal,
            line: self.line,
        }
    }
}
