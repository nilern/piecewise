use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;

use util::SrcPos;

#[derive(Debug)]
pub enum LexicalError {
    UnprecedentedOp(char),
    EmptyTok,
    MalformedTok
}

type LexResult<T> = Result<T, LexicalError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter { Paren, Bracket, Brace }

use self::Delimiter::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Separator { Comma, Semicolon }

use self::Separator::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side { Left, Right }

use self::Side::*;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Precedence { Zero, One, Two, Three, Four, Five, Six, Seven }

impl Precedence {
    fn of(chars: &str) -> LexResult<Precedence> {
        use self::Precedence::*;

        if chars == "=" || chars == "=>" { // HACK
            return Ok(Zero);
        }

        // TODO: actually think about this instead of blindly copying Scala
        match chars.chars().next() {
            Some('|') => Ok(One),
            Some('^') => Ok(Two),
            Some('&') => Ok(Three),
            Some('=') | Some('!') => Ok(Four),
            Some('<') | Some('>') => Ok(Five),
            Some('+') | Some('-') => Ok(Six),
            Some('*') | Some('/') | Some('%') => Ok(Seven),
            Some(c) => Err(LexicalError::UnprecedentedOp(c)),
            None => Err(LexicalError::EmptyTok)
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Tok {
    Name(String),
    Op(String, Precedence),
    Symbol(String),
    Int(isize),

    Char(String),
    String(String),

    Delim(Delimiter, Side),

    Arrow,
    Eq,
    Bar,
    Sep(Separator)
}

type LocTok = (SrcPos, Tok, SrcPos);

const CHAR_TOKENS: &'static str = "()[]{}.;|";

const TOKEN_DELIMS: &'static str = "'\"";

fn is_terminator(c: char) -> bool {
    c.is_whitespace() || CHAR_TOKENS.contains(c) || TOKEN_DELIMS.contains(c)
}

impl Tok {
    fn from_char(c: char) -> LexResult<Tok> {
        use self::Tok::*;

        match c {
            '(' => Ok(Delim(Paren, Left)),
            ')' => Ok(Delim(Paren, Right)),
            '[' => Ok(Delim(Bracket, Left)),
            ']' => Ok(Delim(Bracket, Right)),
            '{' => Ok(Delim(Brace, Left)),
            '}' => Ok(Delim(Brace, Right)),

            ',' => Ok(Sep(Comma)),
            ';' => Ok(Sep(Semicolon)),

            '|' => Ok(Bar),

            _ => Err(LexicalError::MalformedTok)
        }
    }

    fn try_eq_or_arrow(&self) -> Option<Tok> { // HACK
        if let &Tok::Op(ref chars, Precedence::Zero) = self {
            match chars as &str {
                "=" => Some(Tok::Eq),
                "=>" => Some(Tok::Arrow),
                _ => unreachable!()
            }
        } else {
            None
        }
    }
}

impl Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Tok::*;

        match self {
            &Name(_) | &Op(_, _) | &Symbol(_) | &Int(_)
            | &Char(_) | &String(_) => write!(f, "{:?}", *self),

            &Delim(Paren, Left) => write!(f, "("),
            &Delim(Paren, Right) => write!(f, ")"),
            &Delim(Bracket, Left) => write!(f, "["),
            &Delim(Bracket, Right) => write!(f, "]"),
            &Delim(Brace, Left) => write!(f, "{{"),
            &Delim(Brace, Right) => write!(f, "}}"),

            &Sep(Comma) => write!(f, ","),
            &Sep(Semicolon) => write!(f, ";"),
            &Eq => write!(f, "="),
            &Arrow => write!(f, "=>"),
            &Bar => write!(f, "|"),
        }
    }
}

pub struct Lexer<'input> {
    chars: Peekable<CharIndices<'input>>,
    pos: SrcPos
}

impl<'input> Lexer<'input> {
    /// Create a new lexer for lexing the given input string.
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices().peekable(),
            pos: SrcPos::default()
        }
    }

    /// Advance to the next position, updating the line and column on the way.
    fn advance(&mut self) {
        match self.chars.next() {
            Some((i, '\n')) => {
                self.pos = SrcPos {
                    index: i + 1,
                    line: self.pos.line + 1,
                    col: 1
                }
            },
            Some((i, _)) => {
                self.pos = SrcPos {
                    index: i + 1,
                    line: self.pos.line,
                    col: self.pos.col + 1
                }
            },
            None => ()
        }
    }

    /// Read a string upto and not including  a character matching `pred`. Return None if EOF is
    /// encountered before a matching character.
    fn until<F>(&mut self, pred: F) -> Option<String> where F: Fn(char) -> bool {
        let mut acc = String::new();

        while let Some(&(_, c)) = self.chars.peek() {
            if pred(c) {
                return Some(acc);
            }
            self.advance();
            acc.push(c);
        }

        None
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexResult<LocTok>;

    fn next(&mut self) -> Option<Self::Item> {
        self.until(|c| !c.is_whitespace());

        let nc = self.chars.peek().map(|&(_, c)| c);
        nc.map(|c| {
            let start = self.pos;

            match c {
                _ if CHAR_TOKENS.contains(c) => {
                    self.advance();
                    Tok::from_char(c).map(|tok| (start, tok, self.pos))
                },
                '\'' | '"' => {
                    self.advance(); // skip left delimiter
                    let chars = self.until(|d| d == c).ok_or(LexicalError::MalformedTok)?;
                    self.advance(); // skip right delimiter
                    Ok((start,
                        if c == '"' { Tok::String(chars) } else { Tok::Char(chars) },
                        self.pos))
                },
                _ => {
                    let chars = self.until(is_terminator).unwrap_or(String::new());
                    if c.is_digit(10) {
                        Ok((start, Tok::Int(isize::from_str(&chars).unwrap()), self.pos))
                    } else if c.is_alphabetic() || c == '@' {
                        Ok((start, Tok::Name(chars), self.pos))
                    } else {
                        Precedence::of(&chars)
                                  .map(|prec| {
                                      let mut tok = Tok::Op(chars.to_string(), prec);
                                      tok = tok.try_eq_or_arrow().unwrap_or(tok);
                                      (start, tok, self.pos)
                                  })
                    }
                },
            }
        })
    }
}
