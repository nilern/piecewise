module Main where
import Data.Default (def)
import Data.Foldable (traverse_)
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString as B
import Parsing.Parser (expr)
import Parsing.Lexer (Tok(..), TokTag(TokEOF), strToInput, LexicalError)
import Parsing.Indentation (WSLexer, runWSLexer, readToken)
import Alphatize (alphatizeStmt, runAlphatization)
import Interpreter (interpretStmt)
import Interpreter.Env (emptyLexEnv, emptyDynEnv)

data PwError = PwParseError LexicalError
             deriving Show

tokenize :: [Tok] -> WSLexer [Tok]
tokenize toks = do tok <- readToken
                   case tok of
                       Tok TokEOF _ _ _ -> return (tok : toks)
                       Tok _ _ _ _ -> tokenize (tok : toks)

main :: IO ()
main = do input <- strToInput <$> B.getContents
          case runWSLexer (tokenize []) def input of
              Right tokList ->
                  do traverse_ (putStrLn . show) (reverse tokList)
                     case Bf.first PwParseError (runWSLexer expr def input) of
                         Right asts ->
                            let counter = 0
                            in case runAlphatization (traverse alphatizeStmt asts) counter of
                                   Right asts' ->
                                       do print asts'
                                          lEnv <- emptyLexEnv
                                          dEnv <- emptyDynEnv
                                          traverse (interpretStmt lEnv dEnv) asts >>= print
                                   Left err ->
                                       print err
                         Left err ->
                             putStrLn $ "Lexical Error: " ++ show err
              Left err ->
                  putStrLn $ "Lexical Error: " ++ show err
