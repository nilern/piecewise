use std::convert::TryFrom;
use std::fmt::{self, Display};

use combine::{StreamOnce, Positioned, Parser,
              position, value, optional, many, many1, none_of, between};
use combine::char::{char, digit, letter, alpha_num, spaces};
use combine::error::{StringStreamError, FastResult};
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
    // Op(String, Precedence),
    Const(Const)
}

impl Token {
    fn int(digits: String) -> Token { Token::Const(Const::Int(digits.parse().unwrap())) }

    fn string(chars: String) -> Token { Token::Const(Const::String(chars)) }

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
            let mut parser = (position(), choice!(
                char('(').with(value(Token::LParen)),
                char(')').with(value(Token::RParen)),
                char('[').with(value(Token::LBracket)),
                char(']').with(value(Token::RBracket)),
                char('{').with(value(Token::LBrace)),
                char('}').with(value(Token::RBrace)),

                char(',').with(value(Token::Comma)),
                char(';').with(value(Token::Semicolon)),

                (char('='), optional(char('>'))).map(|(_, arrowhead)| if arrowhead.is_some() {
                    Token::DArrow
                } else {
                    Token::Eq
                })
                .or(char('|').map(|_| Token::Bar)),

                many1(digit()).map(Token::int),
                between(char('"'), char('"'), many1(none_of("\"".chars()))).map(Token::string),

                char('$').with(many(alpha_num())).map(Token::Dyn),
                (letter(), many(alpha_num())).map(|(c, mut cs): (_, String)| {
                    cs.insert(0, c);
                    Token::Lex(cs)
                })
            ).skip(spaces()));

            match parser.parse_stream_consumed(&mut self.chars) {
                FastResult::ConsumedOk((pos, token)) => {
                    self.buffer.push((pos, token.clone()));
                    self.token_index += 1;
                    Ok(token)
                },
                FastResult::EmptyOk(_) => unreachable!(),
                FastResult::ConsumedErr(err) => Err(err),
                FastResult::EmptyErr(err) => Err(err.error)
            }
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
