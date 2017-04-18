module Main where
import Data.Default (def)
import Data.Foldable (traverse_)
import qualified Data.ByteString as B
import Parser (expr)
import Lexer (Tok(..), TokTag(TokEOF), strToInput)
import Indentation (WSLexer, runWSLexer, readToken)

tokenize :: [Tok] -> WSLexer [Tok]
tokenize toks = do tok <- readToken
                   case tok of
                       Tok TokEOF _ _ _ -> return (tok : toks)
                       Tok _ _ _ _ -> tokenize (tok : toks)  

main :: IO ()
main = do inputStr <- B.getContents
          let input = strToInput inputStr
          let toks = runWSLexer (tokenize []) def input
          case toks of
              Right tokList ->
                  traverse_ (putStrLn . show) (reverse tokList)
              Left err ->
                  putStrLn $ "Lexical Error: " ++ show err
          let ast = runWSLexer expr def input
          print ast
