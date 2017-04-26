module Main where
import Data.Default (def)
import Data.Foldable (traverse_, foldlM)
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString as B
import Control.Monad.State
import Parser (expr)
import Lexer (Tok(..), TokTag(TokEOF), strToInput, LexicalError)
import Indentation (WSLexer, runWSLexer, readToken)
import Alphatize (PatternError, alphatizeStmt)

data PwError = PwParseError LexicalError
             | PwPatternError PatternError
             deriving Show

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
          let counter = 0
          let asts = Bf.first PwParseError (runWSLexer expr def input)
          let asts' = asts >>= foldlM (\(aasts, i) ast ->
                    Bf.first PwPatternError
                      (do (aast, (i', _)) <- runStateT (alphatizeStmt ast) (i, [])
                          return (aast:aasts, i')))
                    ([], counter)
          print (reverse . fst <$> asts')
