use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenKind},
};

pub fn compile(source: &str) -> Result<Chunk, ()> {
    let mut compiler = Compiler::new(Parser::new(Scanner::new(source.into())));
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
                Some(token) if token.kind != TokenKind::Error => break,
                Some(token) => {
                    let msg = token.lexeme.clone();
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
                .map(|token| token.line.to_string())
                .unwrap_or_else(|| "EOF".to_string())
        );
        match token {
            Some(token) if token.kind != TokenKind::Error => eprint!(" at {}", token.lexeme),
            _ => (),
        }
        eprintln!(": {}", msg);
        self.had_error = true;
    }
}

struct Compiler<'a> {
    parser: Parser<'a>,
    current_chunk: Chunk,
}

type CompilerResult = Result<(), ()>;

impl<'a> Compiler<'a> {
    pub fn new(parser: Parser) -> Compiler {
        Compiler {
            parser,
            current_chunk: Chunk::new(),
        }
    }

    // In the future... the chunk handling code will be "more complicated"
    // it might be a good idea to hand over the current_chunk code to a
    // different struct that will manage the current chunk.
    // It doesn't make sense to have accessor for something's own private data
    // as in, acessing current_chunk directly in an error. Using another struct
    // for this will be cleaner.
    // Note for the future :)
    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.current_chunk
    }

    fn emit_byte(&mut self, byte: OpCode) {
        let line = self
            .parser
            .previous
            .as_ref()
            .expect("No previous token")
            .line;
        let current_chunk = self.current_chunk_mut();
        current_chunk.write_chunk(byte, line);
    }

    fn emit_bytes(&mut self, bytes: &[OpCode]) {
        for byte in bytes {
            self.emit_byte(*byte);
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
    }

    fn compile(mut self) -> Result<Chunk, ()> {
        self.parser.advance();
        self.expression()?;
        self.consume(None, "Expected end of expression.");
        self.end_compiler();

        if self.parser.had_error {
            Err(())
        } else {
            Ok(self.current_chunk)
        }
    }

    fn expression(&mut self) -> CompilerResult {
        todo!()
    }

    fn consume(&mut self, kind: Option<TokenKind>, msg: &str) {
        if self.parser.current.as_ref().map(|token| token.kind) == kind {
            self.parser.advance();
        } else {
            self.parser.error_at_current(msg);
        }
    }
}
