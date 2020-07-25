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
        let constant = current_chunk.add_constant(value);
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
        let prefix_rule = ParseRule::get_rule(previous.map(Token::kind)).prefix;
        match prefix_rule {
            Some(prefix_rule) => prefix_rule(self),
            None => self.parser.error("Expected expression."),
        }

        while precedence <= ParseRule::get_rule(self.current().map(Token::kind)).precedence {
            self.parser.advance();
            let infix_rule = ParseRule::get_rule(self.previous().map(Token::kind))
                .infix
                .unwrap();
            infix_rule(self);
        }
    }

    pub fn compile(mut self) -> Result<Chunk, ()> {
        self.parser.advance();
        expression(&mut self);
        self.consume(None, "Expected end of expression.");
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

type ParseFn = fn(&mut CompilerState);

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    fn get_rule(kind: Option<TokenKind>) -> ParseRule {
        use TokenKind::*;
        let (prefix, infix, precedence): (Option<ParseFn>, Option<ParseFn>, Precedence) = match kind
        {
            Some(LeftParen) => (Some(grouping), None, Precedence::None),
            Some(Minus) => (Some(unary), Some(binary), Precedence::Term),
            Some(Plus) => (None, Some(binary), Precedence::Term),
            Some(Slash) => (None, Some(binary), Precedence::Factor),
            Some(Star) => (None, Some(binary), Precedence::Factor),
            Some(Number) => (Some(number), None, Precedence::None),
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

fn emit_return(state: &mut CompilerState) {
    emit_byte(state, OpCode::Return);
}

fn emit_constant(state: &mut CompilerState, value: Value) {
    let constant = OpCode::Constant(state.make_constant(value));
    emit_byte(state, constant);
}
fn binary(state: &mut CompilerState) {
    let op_token_kind = state.previous().map(Token::kind);

    // uncomment these when done :)
    let rule = ParseRule::get_rule(op_token_kind);
    state.parse_precedence(rule.precedence.next_highest());
    let opcode = match op_token_kind {
        Some(TokenKind::Plus) => OpCode::Add,
        Some(TokenKind::Minus) => OpCode::Subtract,
        Some(TokenKind::Star) => OpCode::Multiply,
        Some(TokenKind::Slash) => OpCode::Divide,
        _ => unreachable!(),
    };
    emit_byte(state, opcode);
}

fn grouping(state: &mut CompilerState) {
    expression(state);
    state.consume(
        Some(TokenKind::RightParen),
        "Expected ')' after expression.",
    );
}

fn number(state: &mut CompilerState) {
    let lexeme = state.previous().map(Token::lexeme).unwrap_or_default();
    let value: Value = lexeme.parse().expect("Invalid number :(");
    emit_constant(state, value);
}

fn unary(state: &mut CompilerState) {
    let op_token_kind = state.previous().map(Token::kind);
    state.parse_precedence(Precedence::Unary);
    match op_token_kind {
        Some(TokenKind::Minus) => emit_byte(state, OpCode::Negate),
        _ => unreachable!(),
    }
}

fn expression(state: &mut CompilerState) {
    state.parse_precedence(Precedence::Assignment)
}
