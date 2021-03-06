use crate::object::Object;
use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
};

pub fn compile(source: &str) -> Result<Chunk, ()> {
    let compiler = CompilerState::new(Parser::new(Scanner::new(source.into())));
    compiler.compile()
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

struct CompilerState<'a> {
    parser: Parser<'a>,
    current_chunk: Chunk,
}

impl<'a> CompilerState<'a> {
    pub fn new(parser: Parser) -> CompilerState {
        CompilerState {
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

    pub fn define_variable(&mut self, global: u8) {
        emit_byte(self, OpCode::DefineGlobal(global));
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

type ParseFn = fn(&mut CompilerState, bool);

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

fn emit_byte(state: &mut CompilerState, byte: OpCode) {
    let line = state.previous().map(Token::line).unwrap_or_default();
    let current_chunk = state.current_chunk_mut();
    current_chunk.write_chunk(byte, line);
}

fn emit_bytes(state: &mut CompilerState, bytes: &[OpCode]) {
    for byte in bytes {
        emit_byte(state, *byte);
    }
}

fn emit_return(state: &mut CompilerState) {
    emit_byte(state, OpCode::Return);
}

fn emit_constant(state: &mut CompilerState, value: Value) {
    let constant = OpCode::Constant(state.make_constant(value));
    emit_byte(state, constant);
}

fn binary(state: &mut CompilerState, _can_assign: bool) {
    use TokenKind::*;
    const EB: fn(&mut CompilerState, OpCode) = emit_byte;
    const EBS: fn(&mut CompilerState, &[OpCode]) = emit_bytes;

    let op_token_kind = state.previous().map(Token::kind).unwrap();

    let rule = ParseRule::get_rule(op_token_kind);
    state.parse_precedence(rule.precedence.next_highest());

    match op_token_kind {
        BangEqual => EBS(state, &[OpCode::Equal, OpCode::Not]),
        EqualEqual => EB(state, OpCode::Equal),
        Greater => EB(state, OpCode::Greater),
        GreaterEqual => EBS(state, &[OpCode::Less, OpCode::Not]),
        Less => EB(state, OpCode::Less),
        LessEqual => EBS(state, &[OpCode::Greater, OpCode::Not]),
        Plus => EB(state, OpCode::Add),
        Minus => EB(state, OpCode::Subtract),
        Star => EB(state, OpCode::Multiply),
        Slash => EB(state, OpCode::Divide),
        _ => unreachable!(),
    };
}

fn literal(state: &mut CompilerState, _can_assign: bool) {
    match state.previous().map(Token::kind) {
        Some(TokenKind::False) => emit_byte(state, OpCode::False),
        Some(TokenKind::Nil) => emit_byte(state, OpCode::Nil),
        Some(TokenKind::True) => emit_byte(state, OpCode::True),
        _ => unreachable!(),
    }
}

fn grouping(state: &mut CompilerState, _can_assign: bool) {
    expression(state);
    state.consume(
        Some(TokenKind::RightParen),
        "Expected ')' after expression.",
    );
}

fn number(state: &mut CompilerState, _can_assign: bool) {
    let lexeme = state.previous().map(Token::lexeme).unwrap_or_default();
    let value = lexeme.parse().expect("Invalid number :(");
    emit_constant(state, Value::Number(value));
}

fn string(state: &mut CompilerState, _can_assign: bool) {
    let s = state.previous().unwrap().lexeme().trim_matches('"');
    let o = Object::String(s.to_string());
    emit_constant(state, Value::Object(o));
}

fn named_variable(state: &mut CompilerState, name: String, can_assign: bool) {
    let arg = state.identifier_constant(name);
    match (can_assign, state.current().map(Token::kind)) {
        (true, Some(TokenKind::Equal)) => {
            state.parser.advance();
            expression(state);
            emit_byte(state, OpCode::SetGlobal(arg));
        }
        _ => emit_byte(state, OpCode::GetGlobal(arg)),
    }
}

fn variable(state: &mut CompilerState, can_assign: bool) {
    // altertaion from book: Token by ref not possible here, clone the lexeme name
    let name = state
        .previous()
        .map(Token::lexeme)
        .map(String::from)
        .unwrap_or_default();
    named_variable(state, name, can_assign);
}

fn unary(state: &mut CompilerState, _can_assign: bool) {
    let op_token_kind = state.previous().map(Token::kind);
    state.parse_precedence(Precedence::Unary);
    match op_token_kind {
        Some(TokenKind::Minus) => emit_byte(state, OpCode::Negate),
        Some(TokenKind::Bang) => emit_byte(state, OpCode::Not),
        _ => unreachable!(),
    }
}

fn expression(state: &mut CompilerState) {
    state.parse_precedence(Precedence::Assignment)
}

fn var_declaration(state: &mut CompilerState) {
    let global = state.parse_variable("Expect variable name.");
    match state.current().unwrap().kind() {
        TokenKind::Equal => {
            state.parser.advance();
            expression(state);
        }
        _ => emit_byte(state, OpCode::Nil),
    }
    state.consume(
        Some(TokenKind::Semicolon),
        "Expect ';' after variable declaration.",
    );
    state.define_variable(global);
}

fn expression_statement(state: &mut CompilerState) {
    expression(state);
    state.consume(Some(TokenKind::Semicolon), "Expect ';' after expression.");
    emit_byte(state, OpCode::Pop)
}

fn declaration(state: &mut CompilerState) {
    match state.current().unwrap().kind() {
        TokenKind::Var => {
            state.parser.advance();
            var_declaration(state);
        }
        _ => statement(state),
    }

    if state.parser.panic_mode {
        state.synchronize();
    }
}

fn statement(state: &mut CompilerState) {
    debug!("statement: {:?}", state.current());
    match state.current().map(Token::kind) {
        Some(TokenKind::Print) => {
            state.parser.advance();
            print_statement(state);
        }
        Some(_) => {
            expression_statement(state);
        }
        None => (),
    }
}

fn print_statement(state: &mut CompilerState) {
    expression(state);
    state.consume(Some(TokenKind::Semicolon), "Expect ';' after value.");
    emit_byte(state, OpCode::Print);
}
