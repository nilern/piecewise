use std::fmt;
use std::fmt::Display;
use std::ops::RangeFrom;

use lexer::{Tok, LexicalError};
use __lalrpop_util::ParseError;
use expand::ExpansionError;
//use resolve::ResolveError;
use value::{TypeError, BoundsError};

/// Values that originated in source code, IR trees and suchlike.
pub trait Sourced {
    /// Return the source position.
    fn pos(&self) -> SrcPos;
}

/// File position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcPos {
    pub index: usize,
    pub line: usize,
    pub col: usize
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

/// Iterator over plentiful stream of unique indices
pub type IndexSrc = RangeFrom<usize>;

/// An identifier/symbol type for use in ASTs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    Simple(String),
    Unique(String, usize)
}

impl Name {
    /// Create unique name based on `chars` with `index_source`.
    pub fn unique(chars: String, index_source: &mut IndexSrc) -> Name {
        Name::Unique(chars, index_source.next().unwrap())
    }

    pub fn as_unique(&self, index_source: &mut IndexSrc) -> Name {
        match self {
            &Name::Simple(ref chars) => Name::unique(chars.clone(), index_source),
            u @ &Name::Unique(..) => u.clone()
        }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Name::Simple(ref chars) => chars.fmt(f),
            &Name::Unique(ref chars, i) => write!(f, "{}{}", chars, i)
        }
    }
}

impl From<String> for Name {
    fn from(chars: String) -> Name {
        Name::Simple(chars)
    }
}

/// Union of most errors in the program.
#[derive(Debug)]
pub enum ProffError {
    Parse(ParseError<SrcPos, Tok, LexicalError>),
    Expansion(ExpansionError),
    //Resolution(ResolveError),
    Type(TypeError),
    Bounds(BoundsError)
}

impl From<ParseError<SrcPos, Tok, LexicalError>> for ProffError {
    fn from(exp_err: ParseError<SrcPos, Tok, LexicalError>) -> ProffError {
        ProffError::Parse(exp_err)
    }
}

impl From<ExpansionError> for ProffError {
    fn from(exp_err: ExpansionError) -> ProffError {
        ProffError::Expansion(exp_err)
    }
}

// impl From<ResolveError> for ProffError {
//     fn from(res_err: ResolveError) -> ProffError {
//         ProffError::Resolution(res_err)
//     }
// }

impl From<TypeError> for ProffError {
    fn from(terr: TypeError) -> ProffError {
        ProffError::Type(terr)
    }
}

impl From<BoundsError> for ProffError {
    fn from(terr: BoundsError) -> ProffError {
        ProffError::Bounds(terr)
    }
}
