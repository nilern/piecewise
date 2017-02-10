use std::str::CharIndices;
use std::iter::Peekable;
use std::collections::VecDeque;
use std::cmp::Ordering;

// TODO: remember whether braces and semicolons resulted from whitespace

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

/// A token with line and column information.
#[derive(Clone, Debug)]
pub struct LocTok {
    pub tok: Tok,
    line: usize,
    col: usize
}

impl LocTok {
    /// Create a `(usize, LocTok, usize)` at the specified position.
    fn at(tok: Tok, start: usize, line: usize, col: usize, end: usize) -> (usize, LocTok, usize) {
        (start, LocTok { tok: tok, line: line, col: col }, end)
    }
}

/// Like a StringBuilder, but for tokens.
struct LocTokBuilder {
    tok: Tok,
    spos: usize,
    epos: Option<usize>,
    line: usize,
    col: usize
}

impl LocTokBuilder {
    /// Start building a Tok::Name.
    fn name(pos: usize, line: usize, col: usize) -> LocTokBuilder {
        LocTokBuilder {
            tok: Tok::Name(String::new()),
            spos: pos,
            epos: None,
            line: line,
            col: col
        }
    }

    // fn char(pos: usize, line: usize, col: usize) -> LocTokBuilder {
    //     LocTokBuilder {
    //         tok: Tok::Char(String::new()),
    //         spos: pos,
    //         epos: None,
    //         line: line,
    //         col: col
    //     }
    // }

    /// Add a character.
    fn push(mut self, c: char) -> LocTokBuilder {
        self.chars_mut().push(c);
        self
    }

    /// Set end position.
    fn ends_at(mut self, i: usize) -> LocTokBuilder {
        self.epos = Some(i);
        self
    }

    /// Build the position-informed token.
    fn build(self) -> (usize, LocTok, usize) {
        let epos = self.epos.unwrap_or_else(|| self.spos + self.chars().len());
        LocTok::at(self.tok, self.spos, self.line, self.col, epos)
    }

    /// Get a reference to the character buffer.
    fn chars(&self) -> &str {
        match self.tok {
            Tok::Name(ref cs) | Tok::Op(ref cs) | Tok::Symbol(ref cs)
            | Tok::Number(ref cs) | Tok::Char(ref cs) | Tok::String(ref cs) => cs,
            _ => unreachable!()
        }
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
    line: usize,
    col: usize
}

impl<'input> Lexer<'input> {
    /// Create a new lexer for lexing the given input string.
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices().peekable(),
            line: 1,
            col: 1
        }
    }

    /// Advance to the next position, updating the line and column on the way.
    fn advance(&mut self) {
        match self.chars.next() {
            Some((_, '\n')) => {
                self.line += 1;
                self.col = 1;
            },
            Some((_, _)) => {
                self.col += 1;
            },
            None => ()
        }
    }

    /// A delimiter or separator was encountered, either build the extended token that was pending
    /// or emit the delimiter/separator.
    fn char_token(&mut self, builder: Option<LocTokBuilder>, tok: Tok, i: usize)
        -> Option<Spanned<LocTok, usize, LexicalError>> {

        match builder {
            Some(builder) => Some(Ok(builder.ends_at(i).build())),
            None => {
                let res = Some(Ok(LocTok::at(tok, i, self.line, self.col, i + 1)));
                self.advance();
                res
            }
        }
    }

    /// Add the whitespace-inferred tokens to this token stream.
    pub fn with_ws_stx(self) -> WSLexer<'input> {
        WSLexer {
            tokens: self.peekable(),
            prev: None,
            pending: VecDeque::new(),
            indents: Vec::new(),
            delimiters: Vec::new(),
            done: false
        }
    }
}


impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<LocTok, usize, LexicalError>;

    // TODO: operators, numbers, characters, strings
    fn next(&mut self) -> Option<Self::Item> {
        let mut acc = None;

        loop {
            match self.chars.peek() {
                Some(&(i, '('))  => return self.char_token(acc, Tok::LParen, i),
                Some(&(i, ')'))  => return self.char_token(acc, Tok::RParen, i),
                Some(&(i, '['))  => return self.char_token(acc, Tok::LBracket, i),
                Some(&(i, ']'))  => return self.char_token(acc, Tok::RBracket, i),
                Some(&(i, '{'))  => return self.char_token(acc, Tok::LBrace, i),
                Some(&(i, '}'))  => return self.char_token(acc, Tok::RBrace, i),

                Some(&(i, ',')) => return self.char_token(acc, Tok::Comma, i),
                Some(&(i, ';')) => return self.char_token(acc, Tok::Semicolon, i),

                Some(&(_, c)) if c.is_whitespace() =>
                    if let Some(b) = acc {
                        return Some(Ok(b.build()));
                    },

                Some(&(i, c)) =>
                    if let Some(b) = acc {
                        acc = Some(b.push(c));
                    } else {
                        acc = Some(LocTokBuilder::name(i, self.line, self.col).push(c));
                    },

                None => return acc.map(|b| Ok(b.build()))
            }
            self.advance();
        }
    }
}

pub struct WSLexer<'input> {
    tokens: Peekable<Lexer<'input>>,
    prev: Option<(usize, usize, usize)>,
    pending: VecDeque<(usize, LocTok, usize)>,
    indents: Vec<usize>,
    delimiters: Vec<Tok>,
    done: bool
}

impl<'input> WSLexer<'input> {
    /// Get the current indentation level.
    fn curr_indent(&self) -> usize {
        *self.indents.last().unwrap_or(&1)
    }

    /// Get the first token in self.pending.
    fn pop(&mut self) -> Option<(usize, LocTok, usize)> {
        self.pending.pop_front()
                    .map(|tok| {
                        self.prev = Some((tok.1.line, tok.1.col, tok.2));
                        tok
                    })
    }

    /// Enqueue a separator corresponding to a newline in the source.
    fn newline(&mut self, start: usize, sline: usize, scol: usize, end: usize) {
        self.pending.push_back(LocTok::at(Tok::Semicolon, start, sline, scol, end))
    }

    /// Enqueue a delimiter corresponding to an indent in the source.
    fn indent(&mut self, start: usize, sline: usize, scol: usize, end: usize, ecol: usize) {
        self.indents.push(ecol);
        self.pending.push_back(LocTok::at(Tok::LBrace, start, sline, scol, end))
    }

    /// Enqueue a delimiter corresponding to an dedent in the source.
    fn dedent(&mut self, start: usize, sline: usize, scol: usize, end: usize) {
        self.indents.pop();
        self.pending.push_back(LocTok::at(Tok::RBrace, start, sline, scol, end))
    }

    /// Enqueue enough dedents to get down to `dest_col`.
    fn dedent_downto(&mut self, dest_col: usize,
                     start: usize, sline: usize, scol: usize, end: usize) {
        while self.curr_indent() > dest_col {
            self.dedent(start, sline, scol, end);
        }
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

    /// Is indentation significant (that is, are we outside all 'concrete' delmiters)?
    fn sig_indents(&self) -> bool {
        self.delimiters.is_empty()
    }

    /// Enqueue the next token from `self.tokens`, possibly preceded by whitespace-inferred tokens.
    /// Returns `Ok(true)` if some tokens were shifted and `Ok(false)` if at EOF.
    fn shift(&mut self) -> Result<bool, LexicalError> {
        match self.tokens.next() {
            Some(Ok(tok)) => {
                if self.sig_indents() {
                    let &(si, LocTok { line, col, .. }, _) = &tok;
                    let (prev_line, prev_col, prev_end) = self.prev.unwrap_or((line, 1, 0));

                    if line > prev_line {
                        match col.cmp(&self.curr_indent()) {
                            Ordering::Equal =>
                                self.newline(prev_end, prev_line, prev_col, si),
                            Ordering::Greater =>
                                self.indent(prev_end, prev_line, prev_col, si, col),
                            Ordering::Less => {
                                self.dedent_downto(col, prev_end, prev_line, prev_col, si);

                                if col > self.curr_indent() {
                                    return Err(LexicalError::WildDedent);
                                } else {
                                    self.newline(prev_end, prev_line, prev_col, si);
                                }
                            }
                        }
                    }
                }

                try!(self.delimiter(&tok.1.tok));

                self.pending.push_back(tok);
                Ok(true)
            },
            Some(Err(err)) => Err(err),
            None if !self.done => {
                let (prev_line, prev_col, prev_end) = self.prev.unwrap_or((1, 1, 0));
                self.dedent_downto(1, prev_end, prev_line, prev_col, prev_end);
                self.done = true;
                Ok(true)
            },
            None => Ok(false)
        }
    }
}

impl<'input> Iterator for WSLexer<'input> {
    type Item = Spanned<LocTok, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop().map(|v| Ok(v))
            .or_else(|| {
                match self.shift() {
                    Ok(true) => self.next(),
                    Ok(false) => None,
                    Err(err) => Some(Err(err))
                }
            })
    }
}
