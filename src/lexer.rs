use std::str::CharIndices;
use std::iter::Peekable;
use std::collections::VecDeque;
use std::cmp::Ordering;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug)]
pub enum LexicalError {
    WildDedent
}

#[derive(Clone, Debug)]
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
    Indent,
    Dedent,
    
    Comma,     // r","
    Semicolon, // r";"
    Newline
}

#[derive(Clone, Debug)]
pub struct LocTok {
    pub tok: Tok,
    line: usize,
    col: usize
}

impl LocTok {
    fn new(tok: Tok, line: usize, col: usize) -> LocTok {
        LocTok { tok: tok, line: line, col: col }
    }
}

struct LocTokBuilder {
    tok: Tok,
    spos: usize,
    epos: Option<usize>,
    line: usize,
    col: usize
}

impl LocTokBuilder {
    fn name(pos: usize, line: usize, col: usize) -> LocTokBuilder {
        LocTokBuilder {
            tok: Tok::Name(String::new()),
            spos: pos,
            epos: None,
            line: line,
            col: col
        }
    }
    
    fn char(pos: usize, line: usize, col: usize) -> LocTokBuilder {
        LocTokBuilder {
            tok: Tok::Char(String::new()),
            spos: pos,
            epos: None,
            line: line,
            col: col
        }
    }

    fn push(mut self, c: char) -> LocTokBuilder {
        self.chars_mut().push(c);
        self
    }

    fn ends_at(mut self, i: usize) -> LocTokBuilder {
        self.epos = Some(i);
        self
    }

    fn build(self) -> (usize, LocTok, usize) {
        let epos = self.epos.unwrap_or_else(|| self.spos + self.chars().len());
        (self.spos, LocTok::new(self.tok, self.line, self.col), epos)
    }
    
    fn chars(&self) -> &String {
        match self.tok {
            Tok::Name(ref cs) | Tok::Op(ref cs) | Tok::Symbol(ref cs)
            | Tok::Number(ref cs) | Tok::Char(ref cs) | Tok::String(ref cs) => cs,
            _ => unreachable!()
        }
    }

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
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices().peekable(),
            line: 1,
            col: 1
        }
    }

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
    
    fn char_token(&mut self, builder: Option<LocTokBuilder>, tok: Tok, i: usize)
        -> Option<Spanned<LocTok, usize, LexicalError>> {

        match builder {
            Some(builder) => Some(Ok(builder.ends_at(i).build())),
            None => {
                let res = Some(Ok((i, LocTok::new(tok, self.line, self.col), i + 1)));
                self.advance();
                res
            }
        }
    }

    pub fn with_ws_stx(self) -> WSLexer<'input> {
        WSLexer {
            tokens: self.peekable(),
            prev: None,
            pending: VecDeque::new(),
            indents: Vec::new()
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

// TODO: ignore indentation inside ()[]{}
pub struct WSLexer<'input> {
    tokens: Peekable<Lexer<'input>>,
    prev: Option<(usize, usize, usize)>,
    pending: VecDeque<(usize, LocTok, usize)>,
    indents: Vec<usize>
}

impl<'input> Iterator for WSLexer<'input> {
    type Item = Spanned<LocTok, usize, LexicalError>;

    // TODO: EOF dedents
    fn next(&mut self) -> Option<Self::Item> {
        if self.pending.is_empty() {
            match self.tokens.next() {
                Some(Ok(tok)) => {
                    let &(si, LocTok { line, col, .. }, _) = &tok;
                    let (prev_line, prev_col, prev_end) = self.prev.unwrap_or((line, 1, 0));
                
                    if tok.1.line > prev_line {
                        match col.cmp(self.indents.last().unwrap_or(&1)) {
                            Ordering::Equal => {
                                self.pending.push_back((prev_end,
                                                LocTok::new(Tok::Newline, prev_line, prev_col),
                                                si));
                                self.pending.push_back(tok);
                            },
                            Ordering::Greater => {
                                self.indents.push(col);
                                self.pending.push_back((prev_end,
                                                LocTok::new(Tok::Indent, prev_line, prev_col),
                                                si));
                                self.pending.push_back(tok);
                            }
                            Ordering::Less => {
                                loop {
                                    match col.cmp(self.indents.last().unwrap_or(&1)) {
                                        Ordering::Equal => break,
                                        Ordering::Less => {
                                            self.indents.pop();
                                            self.pending.push_back(
                                                (prev_end,
                                                 LocTok::new(Tok::Dedent, prev_line, prev_col),
                                                 si));
                                        },
                                        Ordering::Greater =>
                                            return Some(Err(LexicalError::WildDedent))
                                    }
                                }
                                self.pending.push_back(tok);
                            }
                        }
                    } else {
                        self.pending.push_back(tok);
                    }
                }
                err @ Some(Err(_)) => return err,
                None => return None
            }
        }

        let &(_, LocTok { line, col, .. }, ei) = self.pending.front().unwrap();
        self.prev = Some((line, col, ei));
        self.pending.pop_front().map(|v| Ok(v))
    }
}
