{
module Lexer (Tok(TokInt, TokEq, TokPlusEq, TokArrow,
                    TokDelim, TokSemiColon, TokComma),
              Delimiter(Paren, Bracket, Brace), Side(L, R),
              lexer) where
import Data.Char (isAlpha, isSpace, isDigit)
}

%wrapper "basic"

$digit = 0-9

tokens :-
    $white+ ;
    $digit+ { TokInt . read }
    "=>"    { const TokArrow }
    "+="    { const TokPlusEq }
    "="     { const TokEq }
    "{"     { const $ TokDelim Brace L }
    "}"     { const $ TokDelim Brace R }
    ";"     { const TokSemiColon }
    ","     { const TokComma }

{
data Delimiter = Paren | Bracket | Brace deriving Show

data Side = L | R deriving Show

data Precedence = Zero | One | Two | Three | Four | Five | Six | Seven

data Tok = TokInt Int
         | TokEq
         | TokPlusEq
         | TokArrow
         | TokDelim Delimiter Side
         | TokSemiColon
         | TokComma
         deriving Show

lexer :: String -> [Tok]
lexer = alexScanTokens
}
