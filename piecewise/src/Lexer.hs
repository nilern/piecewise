module Lexer (Token(TokenInt, TokenLBrace, TokenRBrace), lexer) where
import Data.Char (isAlpha, isSpace, isDigit)

data Token = TokenInt Int
           | TokenLBrace
           | TokenRBrace
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
lexer ('{':cs) = TokenLBrace : lexer cs
lexer ('}':cs) = TokenRBrace : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
    where (num, rest) = span isDigit cs
