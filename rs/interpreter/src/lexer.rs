use std::fmt::{self, Display};

use combine::{StreamOnce, Positioned, Parser, ParseError, optional, many, many1, none_of, between};
use combine::char::{char, digit, letter, alpha_num, spaces};
use combine::error::StringStreamError;
use combine::stream::state::{State, Positioner};
use combine::stream::easy::Errors;
use combine::stream::{Resetable, StreamErrorFor};

use cst::{Pos, Const};

// ================================================================================================

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimiter { Paren, Bracket, Brace }

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Side { Left, Right }

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Separator { Comma, Semicolon }

#[derive(Debug, Clone, PartialEq)]
pub enum Var { Lex(String), Dyn(String) }

impl Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Var::Lex(ref name) => name.fmt(f),
            &Var::Dyn(ref name) => write!(f, "${}", name)
        }
    }
}

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub enum Precedence { Zero, One, Two, Three, Four, Five, Six, Seven }

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Delimiter(Side, Delimiter),
    Separator(Separator),
    Eq,
    DArrow,
    Bar,

    Var(Var),
    // Op(Var, Precedence),
    Const(Const)
}

impl Token {
    fn dyn(name: String) -> Token { Token::Var(Var::Dyn(name)) }

    fn lex(name: String) -> Token { Token::Var(Var::Lex(name)) }

    fn int(digits: String) -> Token { Token::Const(Const::Int(digits.parse().unwrap())) }

    fn string(chars: String) -> Token { Token::Const(Const::String(chars)) }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Token::*;
        use self::Delimiter::*;
        use self::Side::*;
        use self::Separator::*;

        match self {
            &Delimiter(Left, Paren) => write!(f, "("),
            &Delimiter(Right, Paren) => write!(f, ")"),
            &Delimiter(Left, Bracket) => write!(f, "["),
            &Delimiter(Right, Bracket) => write!(f, "]"),
            &Delimiter(Left, Brace) => write!(f, "{{"),
            &Delimiter(Right, Brace) => write!(f, "}}"),

            &Separator(Comma) => write!(f, ","),
            &Separator(Semicolon) => write!(f, ";"),

            &Eq => write!(f, "="),
            &DArrow => write!(f, "=>"),
            &Bar => write!(f, "|"),

            &Var(ref var) => var.fmt(f),
            // &Op(ref var, _) => var.fmt(f),
            &Const(ref c) => c.fmt(f)
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
    buffer: Vec<Token>,
    token_index: usize
}

impl<'input> Lexer<'input> {
    /// Create a new lexer for lexing the given input string.
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: State::with_positioner(input, Pos::default()),
            buffer: Vec::new(),
            token_index: 0
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> { self.uncons().ok() }
}

impl<'input> StreamOnce for Lexer<'input> {
    type Item = Token;
    type Range = Token; // TODO: &'input [Token]
    type Position = Pos;
    type Error = Errors<Self::Item, Self::Range, Self::Position>;

    fn uncons(&mut self) -> Result<Self::Item, StreamErrorFor<Self>> {
        use self::Side::*;
        use self::Delimiter::*;
        use self::Separator::*;

        if let Some(token) = self.buffer.get(self.token_index).map(Clone::clone) {
            self.token_index += 1;
            Ok(token)
        } else {
            let delimiter = choice!(
                char('(').map(|_| Token::Delimiter(Left, Paren)),
                char(')').map(|_| Token::Delimiter(Right, Paren)),
                char('[').map(|_| Token::Delimiter(Left, Bracket)),
                char(']').map(|_| Token::Delimiter(Right, Bracket)),
                char('{').map(|_| Token::Delimiter(Left, Brace)),
                char('}').map(|_| Token::Delimiter(Right, Brace))
            );
            let separator = choice!(
                char(',').map(|_| Token::Separator(Comma)),
                char(';').map(|_| Token::Separator(Semicolon))
            );
            let special_operator = choice!(
                char('=').and(optional(char('>'))).map(|(_, arrowhead)| if arrowhead.is_some() {
                    Token::DArrow
                } else {
                    Token::Eq
                }),
                char('|').map(|_| Token::Bar)
            );

            let int_parser = many1(digit());
            let string_parser = between(char('"'), char('"'), many1(none_of("\"".chars())));

            let dyn_parser = char('$').with(many(alpha_num()));
            let lex_parser = letter().and(many(alpha_num()));

            let mut parser = spaces().with(choice!(
                delimiter,
                separator,
                special_operator,
                int_parser.map(Token::int),
                string_parser.map(Token::string),
                dyn_parser.map(Token::dyn),
                lex_parser.map(|(c, mut cs): (_, String)| { cs.insert(0, c); Token::lex(cs) })
            ));

            match parser.parse_stream_consumed(&mut self.chars).into() {
                Ok((token, _)) => {
                    self.buffer.push(token.clone());
                    self.token_index += 1;
                    Ok(token)
                },
                Err(err) => Err(<StringStreamError as ParseError<_, _, Pos>>::into_other(
                    err.into_inner().error
                ))
            }
        }
    }
}

impl<'input> Resetable for Lexer<'input> {
    type Checkpoint = usize;

    fn checkpoint(&self) -> Self::Checkpoint { self.token_index }

    fn reset(&mut self, checkpoint: Self::Checkpoint) {
        assert!(checkpoint < self.buffer.len());
        self.token_index = checkpoint
    }
}

impl<'input> Positioned for Lexer<'input> {
    fn position(&self) -> Self::Position { self.chars.position() }
}

// impl Precedence {
//     fn of(chars: &str) -> LexResult<Precedence> {
//         use self::Precedence::*;
//
//         if chars == "=" || chars == "=>" { // HACK
//             return Ok(Zero);
//         }
//
//         // TODO: actually think about this instead of blindly copying Scala
//         match chars.chars().next() {
//             Some('|') => Ok(One),
//             Some('^') => Ok(Two),
//             Some('&') => Ok(Three),
//             Some('=') | Some('!') => Ok(Four),
//             Some('<') | Some('>') => Ok(Five),
//             Some('+') | Some('-') => Ok(Six),
//             Some('*') | Some('/') | Some('%') => Ok(Seven),
//             Some(c) => Err(LexicalError::UnprecedentedOp(c)),
//             None => Err(LexicalError::EmptyTok)
//         }
//     }
// }
//
// fn is_terminator(c: char) -> bool {
//     c.is_whitespace() || CHAR_TOKENS.contains(c) || TOKEN_DELIMS.contains(c)
// }
//
