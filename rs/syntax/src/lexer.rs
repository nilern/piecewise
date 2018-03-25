use std::convert::TryFrom;
use std::fmt::{self, Display};

use combine::{self, StreamOnce, Positioned, Parser};
use combine::char::spaces;
use combine::error::StringStreamError;
use combine::stream::state::{State, Positioner};
use combine::stream::{Resetable, StreamErrorFor};

use cst::{Pos, Const};

// ================================================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen, RParen, LBracket, RBracket, LBrace, RBrace,
    Comma, Semicolon,
    Eq,
    DArrow,
    Bar,

    Lex(String),
    Dyn(String),
    // TODO: Op(String, Precedence),
    Const(Const)
}

impl Token {
    pub fn lex_name(self) -> Option<String> {
        if let Token::Lex(name) = self { Some(name) } else { None }
    }

    pub fn dyn_name(self) -> Option<String> {
        if let Token::Dyn(name) = self { Some(name) } else { None }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Token::*;

        match self {
            &LParen => write!(f, "("),
            &RParen => write!(f, ")"),
            &LBracket => write!(f, "["),
            &RBracket => write!(f, "]"),
            &LBrace => write!(f, "{{"),
            &RBrace => write!(f, "}}"),

            &Comma => write!(f, ","),
            &Semicolon => write!(f, ";"),

            &Eq => write!(f, "="),
            &DArrow => write!(f, "=>"),
            &Bar => write!(f, "|"),

            &Lex(ref name) => name.fmt(f),
            &Dyn(ref name) => write!(f, "${}", name),
            // &Op(ref var, _) => var.fmt(f),
            &Const(ref c) => c.fmt(f)
        }
    }
}

pub struct TryFromTokenError;

impl TryFrom<Token> for Const {
    type Error = TryFromTokenError;

    fn try_from(token: Token) -> Result<Const, TryFromTokenError> {
        if let Token::Const(c) = token {
            Ok(c)
        } else {
            Err(TryFromTokenError)
        }
    }
}

impl From<Token> for combine::error::Info<Token, Token> {
    fn from(token: Token) -> Self { combine::error::Info::Token(token) }
}

// ================================================================================================

impl Positioner<char> for Pos {
    type Position = Pos;

    fn position(&self) -> Pos { self.clone() }

    fn update(&mut self, item: &char) {
        self.index += 1;
        if *item == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }
}

impl Resetable for Pos {
    type Checkpoint = Self;

    fn checkpoint(&self) -> Self { self.clone() }

    fn reset(&mut self, checkpoint: Self) { *self = checkpoint }
}

#[derive(Debug)]
pub struct Lexer<'input> {
    chars: State<&'input str, Pos>,
    buffer: Vec<(Pos, Token)>,
    token_index: usize
}

impl<'input> Lexer<'input> {
    /// Create a new lexer for lexing the given input string.
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: {
                let mut chars = State::with_positioner(input, Pos::default());
                spaces().parse_stream(&mut chars).unwrap();
                chars
            },
            buffer: Vec::new(),
            token_index: 0
        }
    }

    // TODO: Use helper functions instead of manual loops (like in / the ones from `parser`).
    /// Parse one `Token` from `self.chars`.
    fn parse_token(&mut self) -> Result<Token, StreamErrorFor<Self>> {
        let res = self.chars.uncons().and_then(|c| match c {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '[' => Ok(Token::LBracket),
            ']' => Ok(Token::RBracket),
            '{' => Ok(Token::LBrace),
            '}' => Ok(Token::RBrace),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),
            '=' => {
                let checkpoint = self.chars.checkpoint();
                match self.chars.uncons() {
                    Ok('>') => Ok(Token::DArrow),
                    _ => {
                        self.chars.reset(checkpoint);
                        Ok(Token::Eq)
                    }
                }
            },
            '|' => Ok(Token::Bar),
            c if c.is_digit(10) => {
                let mut cs = String::new();
                cs.push(c);
                loop {
                    let checkpoint = self.chars.checkpoint();
                    match self.chars.uncons() {
                        Ok(c) if c.is_digit(10) => cs.push(c),
                        _ => {
                            self.chars.reset(checkpoint);
                            break;
                        }
                    }
                }
                Ok(Token::Const(Const::Int(cs.parse().unwrap())))
            },
            '"' => {
                let mut cs = String::new();
                loop {
                    match self.chars.uncons() {
                        Ok('"') => return Ok(Token::Const(Const::String(cs))),
                        Ok(c) => cs.push(c),
                        Err(err) => return Err(err)
                    }
                }
            },
            '$' => {
                let mut cs = String::new();
                loop {
                    let checkpoint = self.chars.checkpoint();
                    match self.chars.uncons() {
                        Ok(c) if c.is_alphanumeric() => cs.push(c),
                        _ => {
                            self.chars.reset(checkpoint);
                            break;
                        }
                    }
                }
                Ok(Token::Dyn(cs))
            },
            c if c.is_alphabetic() => {
                let mut cs = String::new();
                cs.push(c);
                loop {
                    let checkpoint = self.chars.checkpoint();
                    match self.chars.uncons() {
                        Ok(c) if c.is_alphanumeric() => cs.push(c),
                        _ => {
                            self.chars.reset(checkpoint);
                            break;
                        }
                    }
                }
                Ok(Token::Lex(cs))
            },
            _ => Err(unimplemented!())
        });

        loop {
            let checkpoint = self.chars.checkpoint();
            match self.chars.uncons() {
                Ok(c) if c.is_whitespace() => {},
                _ => {
                    self.chars.reset(checkpoint);
                    break;
                }
            }
        }

        res
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Token, StreamErrorFor<Self>>;

    fn next(&mut self) -> Option<Result<Token, StreamErrorFor<Self>>> {
        Some(self.uncons())
    }
}

impl<'input> StreamOnce for Lexer<'input> {
    type Item = Token;
    type Range = Token; // TODO: &'input [Token]
    type Position = Pos;
    type Error = StringStreamError;

    fn uncons(&mut self) -> Result<Self::Item, StreamErrorFor<Self>> {
        if let Some((_, token)) = self.buffer.get(self.token_index).map(Clone::clone) {
            self.token_index += 1;
            Ok(token)
        } else if self.chars.input.is_empty() {
            Err(StringStreamError::Eoi)
        } else {
            let pos = self.chars.position();
            let tok = self.parse_token()?;
            self.buffer.push((pos, tok.clone()));
            self.token_index += 1;
            Ok(tok)
        }
    }
}

impl<'input> Resetable for Lexer<'input> {
    type Checkpoint = usize;

    fn checkpoint(&self) -> Self::Checkpoint { self.token_index }

    fn reset(&mut self, checkpoint: Self::Checkpoint) {
        assert!(checkpoint <= self.buffer.len());
        self.token_index = checkpoint
    }
}

impl<'input> Positioned for Lexer<'input> {
    fn position(&self) -> Self::Position {
        if self.token_index < self.buffer.len() {
            self.buffer[self.token_index].0.clone()
        } else {
            self.chars.position()
        }
    }
}
