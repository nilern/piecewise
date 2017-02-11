use std::str::CharIndices;
use std::iter::Peekable;
use std::collections::VecDeque;
use std::cmp::Ordering;

// TODO: remember whether braces and semicolons resulted from whitespace

#[derive(Debug, Clone, Copy)]
pub struct SrcPos {
    index: usize,
    line: usize,
    col: usize
}

impl Default for SrcPos {
    fn default() -> SrcPos {
        SrcPos {
            index: 0,
            line: 1,
            col: 1
        }
    }
}

type LocTok = (SrcPos, Tok, SrcPos);
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug)]
pub enum LexicalError {
    WildDedent,
    Delimiter(Tok, Option<Tok>)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Tok {
    Name(String),   // r"[\p{Alphabetic}_@][^\s'\"\(\)\[\]\{\},;]*"
    Op(String),     // _
    Symbol(String), // r":[^\s'\"\(\)\[\]\{\},;]+"
    Number(String), // r"\d[^\s'\"\(\)\[\]\{\},;]*"
    Char(String),   // r"'[^']+'"
    String(String), // r"\"[^\"]\""

    LParen,   // r"("
    RParen,   // r")"
    LBracket, // r"["
    RBracket, // r"]"
    LBrace,   // r"{"
    RBrace,   // r"}"
    // Indent,
    // Dedent,

    Comma,     // r","
    Semicolon, // r";"
    // Newline

    Eq // r"="
}

impl Tok {
    /// Get the matching delimiter, if applicable.
    fn pair(&self) -> Option<Tok> {
        match *self {
            Tok::LParen => Some(Tok::RParen),
            Tok::RParen => Some(Tok::LParen),
            Tok::LBracket => Some(Tok::RBracket),
            Tok::RBracket => Some(Tok::LBracket),
            Tok::LBrace => Some(Tok::RBrace),
            Tok::RBrace => Some(Tok::LBrace),
            _ => None
        }
    }
}

/// Like a StringBuilder, but for tokens.
struct TokBuilder {
    tok: Tok,
    start: SrcPos,
    end: Option<SrcPos>
}

impl TokBuilder {
    /// Start building a Tok::Name.
    fn name(pos: SrcPos) -> TokBuilder {
        TokBuilder {
            tok: Tok::Name(String::new()),
            start: pos,
            end: None
        }
    }

    /// Start building a Tok::Op.
    fn op(pos: SrcPos) -> TokBuilder {
        TokBuilder {
            tok: Tok::Op(String::new()),
            start: pos,
            end: None
        }
    }

    /// Start building a Tok::Number.
    fn number(pos: SrcPos) -> TokBuilder {
        TokBuilder {
            tok: Tok::Number(String::new()),
            start: pos,
            end: None
        }
    }

    /// Add a character.
    fn push(mut self, c: char) -> TokBuilder {
        self.chars_mut().push(c);
        self
    }

    /// Set end position.
    fn ends_at(mut self, pos: SrcPos) -> TokBuilder {
        self.end = Some(pos);
        self
    }

    /// Build the position-informed token.
    fn build(self) -> LocTok {
        let end = self.end.unwrap_or_else(|| self.start); // HACK
        (self.start, self.tok, end)
    }

    /// Get a mutable reference to the character buffer.
    fn chars_mut(&mut self) -> &mut String {
        match self.tok {
            Tok::Name(ref mut cs) | Tok::Op(ref mut cs) | Tok::Symbol(ref mut cs)
            | Tok::Number(ref mut cs) | Tok::Char(ref mut cs) | Tok::String(ref mut cs) => cs,
            _ => unreachable!()
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
            pos: SrcPos { index: 0, line: 1, col: 1}
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

    /// A delimiter or separator was encountered, either build the extended token that was pending
    /// or emit the delimiter/separator.
    fn char_token(&mut self, builder: Option<TokBuilder>, tok: Tok)
        -> Option<Spanned<Tok, SrcPos, LexicalError>> {

        match builder {
            Some(builder) => Some(Ok(builder.ends_at(self.pos).build())),
            None => {
                let start = self.pos;
                self.advance();
                let end = self.pos;
                Some(Ok((start, tok, end)))
            }
        }
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
    type Item = Spanned<Tok, SrcPos, LexicalError>;

    // TODO: operators, numbers, characters, strings
    fn next(&mut self) -> Option<Self::Item> {
        let mut acc = None;

        loop {
            match self.chars.peek() {
                Some(&(_, '('))  => return self.char_token(acc, Tok::LParen,),
                Some(&(_, ')'))  => return self.char_token(acc, Tok::RParen),
                Some(&(_, '['))  => return self.char_token(acc, Tok::LBracket),
                Some(&(_, ']'))  => return self.char_token(acc, Tok::RBracket),
                Some(&(_, '{'))  => return self.char_token(acc, Tok::LBrace,),
                Some(&(_, '}'))  => return self.char_token(acc, Tok::RBrace),

                Some(&(_, ',')) => return self.char_token(acc, Tok::Comma),
                Some(&(_, ';')) => return self.char_token(acc, Tok::Semicolon),

                Some(&(_, '=')) => return self.char_token(acc, Tok::Eq),

                Some(&(_, c)) if c.is_whitespace() =>
                    if let Some(b) = acc {
                        return Some(Ok(b.build()));
                    },

                Some(&(_, c)) =>
                    if let Some(b) = acc {
                        acc = Some(b.push(c));
                    } else if c.is_alphabetic() || c == '@' {
                        acc = Some(TokBuilder::name(self.pos).push(c));
                    } else if c.is_digit(10) {
                        acc = Some(TokBuilder::number(self.pos).push(c));
                    } else {
                        acc = Some(TokBuilder::op(self.pos).push(c))
                    },

                None => return acc.map(|b| Ok(b.build()))
            }
            self.advance();
        }
    }
}

pub struct WSLexer<'input> {
    tokens: Peekable<Lexer<'input>>,
    prev_end: Option<SrcPos>,
    pending: VecDeque<LocTok>,
    indents: Vec<usize>,
    delimiters: Vec<Tok>,
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
        self.pending.push_back((start, Tok::Semicolon, end))
    }

    /// Enqueue a delimiter corresponding to an indent in the source.
    fn indent(&mut self, start: SrcPos, end: SrcPos) {
        self.indents.push(end.col);
        self.pending.push_back((start, Tok::LBrace, end))
    }

    /// Enqueue a delimiter corresponding to an dedent in the source.
    fn dedent(&mut self, start: SrcPos, end: SrcPos) {
        self.indents.pop();
        self.pending.push_back((start, Tok::RBrace, end))
    }

    /// Enqueue enough dedents to get down to `dest_col`.
    fn dedent_downto(&mut self, dest_col: usize, start: SrcPos, end: SrcPos) {
        while self.curr_indent() > dest_col {
            self.dedent(start, end);
        }
    }

    /// Enqueue relevant whitespace tokens based on `self.prev_end` and `start`.
    fn enqueue_ws_tokens(&mut self, start: SrcPos) -> Result<(), LexicalError> {
        if self.sig_indents() {
            let prev_end = self.prev_end(SrcPos { index: 0, line: start.line, col: 1 });

            if start.line > prev_end.line {
                match start.col.cmp(&self.curr_indent()) {
                    Ordering::Equal =>
                        self.newline(prev_end, start),
                    Ordering::Greater =>
                        self.indent(prev_end, start),
                    Ordering::Less => {
                        self.dedent_downto(start.col, prev_end, start);

                        if start.col > self.curr_indent() {
                            return Err(LexicalError::WildDedent);
                        } else {
                            self.newline(prev_end, start);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Push a delimiter to or pop its pair from the delimiter stack.
    fn delimiter(&mut self, tok: &Tok) -> Result<(), LexicalError> {
        match tok {
            &Tok::LParen | &Tok::LBracket | &Tok::LBrace =>
                self.delimiters.push(tok.clone()),
            &Tok::RParen | &Tok::RBracket | &Tok::RBrace =>
                if !self.delimiters.is_empty() {
                    let expected = self.delimiters.last().unwrap().pair().unwrap();
                    if *tok == expected {
                        self.delimiters.pop();
                    } else {
                        return Err(LexicalError::Delimiter(tok.clone(), Some(expected)));
                    }
                } else {
                    return Err(LexicalError::Delimiter(tok.clone(), None));
                },
            _ => {}
        }
        Ok(())
    }

    /// Enqueue the next token from `self.tokens`, possibly preceded by whitespace-inferred tokens.
    /// Returns `Ok(true)` if some tokens were shifted and `Ok(false)` if at EOF.
    fn shift(&mut self) -> Result<bool, LexicalError> {
        match self.tokens.next() {
            Some(Ok(tok)) => {
                try!(self.enqueue_ws_tokens(tok.0));
                try!(self.delimiter(&tok.1));
                self.pending.push_back(tok);
                Ok(true)
            },
            Some(Err(err)) => Err(err),
            None if !self.done => {
                let prev_end = self.prev_end(SrcPos { index: 0, line: 0, col: 0 });
                self.dedent_downto(1, prev_end, prev_end);
                self.done = true;
                Ok(true)
            },
            None => Ok(false)
        }
    }
}

impl<'input> Iterator for WSLexer<'input> {
    type Item = Spanned<Tok, SrcPos, LexicalError>;

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
