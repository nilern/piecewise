module Main where
import qualified Data.ByteString as B
import Parser (expr)
import qualified Lexer

main :: IO ()
main = do input <- B.getContents
          print $ Lexer.lex expr (input, Lexer.Pos 0)
