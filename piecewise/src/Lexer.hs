module Lexer (Token(TokenInt, TokenEq, TokenPlusEq, TokenArrow,
                    TokenLBrace, TokenRBrace, TokenSemiColon, TokenComma),
              lexer) where
import Data.Char (isAlpha, isSpace, isDigit)

data Token = TokenInt Int
           | TokenEq
           | TokenPlusEq
           | TokenArrow
           | TokenLBrace
           | TokenRBrace
           | TokenSemiColon
           | TokenComma
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
lexer ('=':'>':cs) = TokenArrow : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':'=':cs) = TokenPlusEq : lexer cs
lexer ('{':cs) = TokenLBrace : lexer cs
lexer ('}':cs) = TokenRBrace : lexer cs
lexer (';':cs) = TokenSemiColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
    where (num, rest) = span isDigit cs
