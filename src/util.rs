use lexer::{SrcPos, Tok, LexicalError};
use __lalrpop_util::ParseError;
use expand::ExpansionError;

#[derive(Debug)]
pub enum ProffError {
    Parse(ParseError<SrcPos, Tok, LexicalError>),
    Expansion(ExpansionError)
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
