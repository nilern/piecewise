module Main where
import Lexer (lexer)
import Parser (expr)

main :: IO ()
main = getContents >>= print . expr . lexer
