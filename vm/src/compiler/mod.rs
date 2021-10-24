use crate::object::Object;
use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
};

pub fn compile(source: &str) -> Result<Chunk, ()> {
    let c = Compiler::new(Parser::new(Scanner::new(source.into())));
    c.compile()
}

struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new(scanner: Scanner) -> Parser {
        Parser {
            scanner,
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn advance(&mut self) {
        self.previous = self.current.take();
        loop {
            self.current = self.scanner.next();
            match &self.current {
                Some(token) if token.kind() != TokenKind::Error => break,
                Some(token) => {
                    let msg = token.lexeme().to_string();
                    self.error_at_current(&msg);
                }
                None => break,
            }
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current.clone(), msg);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous.clone(), msg);
    }

    fn error_at(&mut self, token: Option<Token>, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!(
            "[{}] Error",
            token
                .as_ref()
                .map(|token| token.line().to_string())
                .unwrap_or_else(|| "EOF".to_string())
        );
        match token {
            Some(token) if token.kind() != TokenKind::Error => eprint!(" at {}", token.lexeme()),
            _ => (),
        }
        eprintln!(": {}", msg);
        self.had_error = true;
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next_highest(self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

type ParseFn = fn(&mut Compiler, bool);

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    fn get_rule_opt(kind: Option<TokenKind>) -> ParseRule {
        match kind {
            Some(kind) => ParseRule::get_rule(kind),
            None => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    fn get_rule(kind: TokenKind) -> ParseRule {
        use TokenKind::*;
        type ParseRule3Tuple = (Option<ParseFn>, Option<ParseFn>, Precedence);

        let (prefix, infix, precedence): ParseRule3Tuple = match kind {
            LeftParen => (Some(grouping), None, Precedence::None),
            Minus => (Some(unary), Some(binary), Precedence::Term),
            Plus => (None, Some(binary), Precedence::Term),
            Slash | Star => (None, Some(binary), Precedence::Factor),
            Bang => (Some(unary), None, Precedence::None),
            BangEqual | EqualEqual => (None, Some(binary), Precedence::Equality),
            Greater | GreaterEqual | Less | LessEqual => {
                (None, Some(binary), Precedence::Comparison)
            }
            False | True | Nil => (Some(literal), None, Precedence::None),
            Number => (Some(number), None, Precedence::None),
            Str => (Some(string), None, Precedence::None),
            Identifier => (Some(variable), None, Precedence::None),
            _ => (None, None, Precedence::None),
        };

        ParseRule {
            prefix,
            infix,
            precedence,
        }
    }
}

struct Compiler<'a> {
    parser: Parser<'a>,
    current_chunk: Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(parser: Parser) -> Compiler {
        Compiler {
            parser,
            current_chunk: Chunk::new(),
        }
    }

    pub fn current_chunk(&self) -> &Chunk {
        &self.current_chunk
    }

    // In the future... the chunk handling code will be "more complicated"
    // it might be a good idea to hand over the current_chunk code to a
    // different struct that will manage the current chunk.
    // It doesn't make sense to have accessor for something's own private data
    // as in, acessing current_chunk directly in an error. Using another struct
    // for this will be cleaner.
    // Note for the future :)
    pub fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.current_chunk
    }

    pub fn current(&self) -> Option<&Token> {
        self.parser.current.as_ref()
    }

    pub fn previous(&self) -> Option<&Token> {
        self.parser.previous.as_ref()
    }

    pub fn make_constant(&mut self, value: Value) -> u8 {
        let current_chunk = self.current_chunk_mut();
        let constant = current_chunk.add_constant(Some(value));
        match constant {
            Ok(c) => c,
            Err(_) => {
                self.parser.error("Too many constants in one chunk");
                0
            }
        }
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) {
        self.parser.advance();
        let previous = self.previous();
        let prefix_rule = ParseRule::get_rule_opt(previous.map(Token::kind)).prefix;
        let can_assign = precedence <= Precedence::Assignment;
        match prefix_rule {
            Some(prefix_rule) => {
                prefix_rule(self, can_assign);
            }
            None => self.parser.error("Expected expression."),
        }

        while precedence <= ParseRule::get_rule_opt(self.current().map(Token::kind)).precedence {
            self.parser.advance();
            let infix_rule = ParseRule::get_rule_opt(self.previous().map(Token::kind))
                .infix
                .unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.current().map(Token::kind) == Some(TokenKind::Equal) {
            self.parser.advance();
            self.parser.error("Invalid assignment target.");
        }
    }

    pub fn identifier_constant(&mut self, name: String) -> u8 {
        self.make_constant(Value::Object(Object::String(name)))
    }

    pub fn parse_variable(&mut self, err_msg: &str) -> u8 {
        self.consume(Some(TokenKind::Identifier), err_msg);
        let name = self.previous().unwrap().lexeme().to_owned();
        self.identifier_constant(name)
    }

    pub fn define_variable(&mut self, line: usize, global: u8) {
        self.emit_byte(line, OpCode::DefineGlobal(global));
    }

    fn emit_byte(&mut self, line: usize, byte: OpCode) {
        self.current_chunk_mut().write_chunk(byte, line);
    }

    // fn emit_bytes(c: &mut Compiler, bytes: &[OpCode]) {
    //     for byte in bytes {
    //         emit_byte(c, *byte);
    //     }
    // }

    pub fn emit_return(&mut self, line: usize) {
        self.emit_byte(line, OpCode::Return);
    }

    fn emit_constant(&mut self, line: usize, value: Value) {
        let constant = OpCode::Constant(self.make_constant(value));
        self.emit_byte(line, constant);
    }

    pub fn compile(mut self) -> Result<Chunk, ()> {
        self.parser.advance();

        while self.current().is_some() {
            declaration(&mut self);
        }

        self.end_compiler();

        if self.parser.had_error {
            Err(())
        } else {
            Ok(self.current_chunk)
        }
    }

    fn end_compiler(&mut self) {
        emit_return(self);

        if !self.parser.had_error {
            debug!("\n----- code -----\n{}", self.current_chunk())
        }
    }

    pub fn consume(&mut self, kind: Option<TokenKind>, msg: &str) {
        if self.current().map(Token::kind) == kind {
            self.parser.advance();
        } else {
            self.parser.error_at_current(msg);
        }
    }

    pub fn synchronize(&mut self) {
        self.parser.panic_mode = false;
        while self.current().is_some() {
            if TokenKind::Semicolon == self.previous().unwrap().kind() {
                return;
            }
            match self.current().unwrap().kind() {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                _ => (),
            }
            self.parser.advance();
        }
    }
}

fn binary(c: &mut Compiler, _can_assign: bool) {
    use TokenKind::*;
    const EB: fn(&mut Compiler, OpCode) = emit_byte;
    const EBS: fn(&mut Compiler, &[OpCode]) = emit_bytes;

    let op_token_kind = c.previous().map(Token::kind).unwrap();

    let rule = ParseRule::get_rule(op_token_kind);
    c.parse_precedence(rule.precedence.next_highest());

    match op_token_kind {
        BangEqual => EBS(c, &[OpCode::Equal, OpCode::Not]),
        EqualEqual => EB(c, OpCode::Equal),
        Greater => EB(c, OpCode::Greater),
        GreaterEqual => EBS(c, &[OpCode::Less, OpCode::Not]),
        Less => EB(c, OpCode::Less),
        LessEqual => EBS(c, &[OpCode::Greater, OpCode::Not]),
        Plus => EB(c, OpCode::Add),
        Minus => EB(c, OpCode::Subtract),
        Star => EB(c, OpCode::Multiply),
        Slash => EB(c, OpCode::Divide),
        _ => unreachable!(),
    };
}

fn literal(c: &mut Compiler, _can_assign: bool) {
    match c.previous().map(Token::kind) {
        Some(TokenKind::False) => emit_byte(c, OpCode::False),
        Some(TokenKind::Nil) => emit_byte(c, OpCode::Nil),
        Some(TokenKind::True) => emit_byte(c, OpCode::True),
        _ => unreachable!(),
    }
}

fn grouping(c: &mut Compiler, _can_assign: bool) {
    expression(c);
    c.consume(
        Some(TokenKind::RightParen),
        "Expected ')' after expression.",
    );
}

fn number(c: &mut Compiler, _can_assign: bool) {
    let lexeme = c.previous().map(Token::lexeme).unwrap_or_default();
    let value = lexeme.parse().expect("Invalid number :(");
    emit_constant(c, Value::Number(value));
}

fn string(c: &mut Compiler, _can_assign: bool) {
    let s = c.previous().unwrap().lexeme().trim_matches('"');
    let o = Object::String(s.to_string());
    emit_constant(c, Value::Object(o));
}

fn named_variable(c: &mut Compiler, name: String, can_assign: bool) {
    let arg = c.identifier_constant(name);
    match (can_assign, c.current().map(Token::kind)) {
        (true, Some(TokenKind::Equal)) => {
            c.parser.advance();
            expression(c);
            emit_byte(c, OpCode::SetGlobal(arg));
        }
        _ => emit_byte(c, OpCode::GetGlobal(arg)),
    }
}

fn variable(c: &mut Compiler, can_assign: bool) {
    // altertaion from book: Token by ref not possible here, clone the lexeme name
    let name = c
        .previous()
        .map(Token::lexeme)
        .map(String::from)
        .unwrap_or_default();
    named_variable(c, name, can_assign);
}

fn unary(c: &mut Compiler, _can_assign: bool) {
    let op_token_kind = c.previous().map(Token::kind);
    c.parse_precedence(Precedence::Unary);
    match op_token_kind {
        Some(TokenKind::Minus) => emit_byte(c, OpCode::Negate),
        Some(TokenKind::Bang) => emit_byte(c, OpCode::Not),
        _ => unreachable!(),
    }
}

fn expression(c: &mut Compiler) {
    c.parse_precedence(Precedence::Assignment)
}

fn var_declaration(c: &mut Compiler) {
    let global = c.parse_variable("Expect variable name.");
    match c.current().unwrap().kind() {
        TokenKind::Equal => {
            c.parser.advance();
            expression(c);
        }
        _ => emit_byte(c, OpCode::Nil),
    }
    c.consume(
        Some(TokenKind::Semicolon),
        "Expect ';' after variable declaration.",
    );
    c.define_variable(global);
}

fn expression_statement(c: &mut Compiler) {
    expression(c);
    c.consume(Some(TokenKind::Semicolon), "Expect ';' after expression.");
    emit_byte(c, OpCode::Pop)
}

fn declaration(c: &mut Compiler) {
    match c.current().unwrap().kind() {
        TokenKind::Var => {
            c.parser.advance();
            var_declaration(c);
        }
        _ => statement(c),
    }

    if c.parser.panic_mode {
        c.synchronize();
    }
}

fn statement(c: &mut Compiler) {
    debug!("statement: {:?}", c.current());
    match c.current().map(Token::kind) {
        Some(TokenKind::Print) => {
            c.parser.advance();
            print_statement(c);
        }
        Some(_) => {
            expression_statement(c);
        }
        None => (),
    }
}

fn print_statement(c: &mut Compiler) {
    expression(c);
    c.consume(Some(TokenKind::Semicolon), "Expect ';' after value.");
    emit_byte(c, OpCode::Print);
}
