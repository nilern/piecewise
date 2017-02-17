use std::str::CharIndices;
use std::iter::Peekable;
use std::collections::VecDeque;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Display;

// TODO: remember whether braces and semicolons resulted from whitespace

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcPos {
    index: usize,
    line: usize,
    col: usize
}

impl Default for SrcPos {
    fn default() -> SrcPos {
        SrcPos { index: 0, line: 1, col: 1 }
    }
}

impl Display for SrcPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{},{}:{}", self.index, self.line, self.col)
    }
}

#[derive(Debug)]
pub enum LexicalError {
    WildDedent,
    Delim(Delimiter, Option<Delimiter>),
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
    Number(String),

    Char(String),
    String(String),

    Delim(Delimiter, Side),

    Sep(Separator)
}

type LocTok = (SrcPos, Tok, SrcPos);

const CHAR_TOKENS: &'static str = "()[]{}.;";

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

            _ => Err(LexicalError::MalformedTok)
        }
    }
}

impl Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Tok::*;

        match self {
            &Name(_) | &Op(_, _) | &Symbol(_) | &Number(_)
            | &Char(_) | &String(_) => write!(f, "{:?}", *self),

            &Delim(Paren, Left) => write!(f, "("),
            &Delim(Paren, Right) => write!(f, ")"),
            &Delim(Bracket, Left) => write!(f, "["),
            &Delim(Bracket, Right) => write!(f, "]"),
            &Delim(Brace, Left) => write!(f, "{{"),
            &Delim(Brace, Right) => write!(f, "}}"),

            &Sep(Comma) => write!(f, ","),
            &Sep(Semicolon) => write!(f, ";")
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

    /// Add the whitespace-inferred tokens to this token stream.
    pub fn with_ws_stx(self) -> WSLexer<'input> {
        WSLexer {
            tokens: self.peekable(),
            prev_end: None,
            pending: VecDeque::new(),
            indents: Vec::new(),
            delimiters: Vec::new(),
            done: false
        }
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
                        Ok((start, Tok::Number(chars), self.pos))
                    } else if c.is_alphabetic() || c == '@' {
                        Ok((start, Tok::Name(chars), self.pos))
                    } else {
                        Precedence::of(&chars)
                                  .map(|prec| (start, Tok::Op(chars.to_string(), prec), self.pos))
                    }
                },
            }
        })
    }
}

pub struct WSLexer<'input> {
    tokens: Peekable<Lexer<'input>>,
    prev_end: Option<SrcPos>,
    pending: VecDeque<LocTok>,
    indents: Vec<usize>,
    delimiters: Vec<Delimiter>,
    done: bool
}

impl<'input> WSLexer<'input> {
    /// Is indentation significant (that is, are we outside all 'concrete' delmiters)?
    fn sig_indents(&self) -> bool {
        self.delimiters.is_empty()
    }

    /// Get the current indentation level.
    fn curr_indent(&self) -> usize {
        *self.indents.last().unwrap_or(&1)
    }

    /// Get the end position of the previous token.
    fn prev_end(&self, default: SrcPos) -> SrcPos {
        self.prev_end.unwrap_or(default)
    }

    /// Get the first token in self.pending.
    fn pop(&mut self) -> Option<LocTok> {
        self.pending.pop_front()
                    .map(|tok| {
                        self.prev_end = Some(tok.2);
                        tok
                    })
    }

    /// Enqueue a separator corresponding to a newline in the source.
    fn newline(&mut self, start: SrcPos, end: SrcPos) {
        self.pending.push_back((start, Tok::Sep(Semicolon), end))
    }

    /// Enqueue a delimiter corresponding to an indent in the source.
    fn indent(&mut self, start: SrcPos, end: SrcPos) {
        self.indents.push(end.col);
        self.pending.push_back((start, Tok::Delim(Brace, Left), end))
    }

    /// Enqueue a delimiter corresponding to an dedent in the source.
    fn dedent(&mut self, start: SrcPos, end: SrcPos) {
        self.indents.pop();
        self.pending.push_back((start, Tok::Delim(Brace, Right), end))
    }

    /// Enqueue enough dedents to get down to `dest_col`.
    fn dedent_downto(&mut self, dest_col: usize, start: SrcPos, end: SrcPos)
        -> Result<(), LexicalError> {

        while self.curr_indent() > dest_col {
            self.dedent(start, end);
        }

        if dest_col > self.curr_indent() {
            Err(LexicalError::WildDedent)
        } else {
            Ok(())
        }
    }

    /// Enqueue relevant whitespace tokens based on `self.prev_end` and `start`.
    fn enqueue_ws_tokens(&mut self, start: SrcPos) -> Result<(), LexicalError> {
        if self.sig_indents() {
            let prev_end = self.prev_end(SrcPos { index: 0, line: start.line, col: 1 });

            if start.line > prev_end.line {
                match start.col.cmp(&self.curr_indent()) {
                    Ordering::Equal => self.newline(prev_end, start),
                    Ordering::Greater => self.indent(prev_end, start),
                    Ordering::Less => {
                        self.dedent_downto(start.col, prev_end, start)?;
                        self.newline(prev_end, start);
                    }
                }
            }
        }
        Ok(())
    }

    /// Push a delimiter to or pop its pair from the delimiter stack.
    fn delimiter(&mut self, tok: &Tok) -> Result<(), LexicalError> {
        match tok {
            &Tok::Delim(delim, Side::Left) => Ok(self.delimiters.push(delim)),
            &Tok::Delim(ref rd, Side::Right) if !self.delimiters.is_empty() =>
                if rd == self.delimiters.last().unwrap() {
                    self.delimiters.pop();
                    Ok(())
                } else {
                    Err(LexicalError::Delim(*rd, Some(self.delimiters.last().unwrap().clone())))
                },
            &Tok::Delim(delim, Side::Right) => Err(LexicalError::Delim(delim, None)),
            _ => Ok(())
        }
    }

    /// Enqueue the next token from `self.tokens`, possibly preceded by whitespace-inferred tokens.
    /// Returns `Ok(true)` if some tokens were shifted and `Ok(false)` if at EOF.
    fn shift(&mut self) -> LexResult<bool> {
        match self.tokens.next() {
            Some(Ok(tok)) => {
                self.enqueue_ws_tokens(tok.0)?;
                self.delimiter(&tok.1)?;
                self.pending.push_back(tok);
                Ok(true)
            },
            Some(Err(err)) => Err(err),
            None if !self.done => {
                let prev_end = self.prev_end(SrcPos { index: 0, line: 0, col: 0 });
                self.dedent_downto(1, prev_end, prev_end)?;
                self.done = true;
                Ok(true)
            },
            None => Ok(false)
        }
    }
}

impl<'input> Iterator for WSLexer<'input> {
    type Item = LexResult<LocTok>;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop()
            .map(|v| Ok(v))
            .or_else(|| {
                match self.shift() {
                    Ok(true) => self.next(),
                    Ok(false) => None,
                    Err(err) => Some(Err(err))
                }
            })
    }
}
