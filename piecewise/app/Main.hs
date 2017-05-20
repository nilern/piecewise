module Main where
import Data.Default (def)
import Data.Foldable (traverse_)
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString as B
import Parsing.Parser (expr)
import Parsing.Lexer (Tok(..), TokTag(TokEOF), strToInput, LexicalError)
import Parsing.Indentation (WSLexer, runWSLexer, readToken)
-- import HoistAugs (ReAssignment, hoistAugStmts)
-- import Alphatize (PatternError)
import Interpreter (interpretStmt)
import Interpreter.Env (emptyLexEnv, emptyDynEnv)

data PwError = PwParseError LexicalError
             -- | PwPatternError PatternError
             -- | PwHoistError ReAssignment
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
                             do lEnv <- emptyLexEnv
                                dEnv <- emptyDynEnv
                                traverse (interpretStmt lEnv dEnv) asts
                                >>= print
                                -- case hoistAugStmts asts of
                                --     Right asts' ->
                                --         do res <- (traverse (interpretStmt lEnv dEnv)
                                --                             asts')
                                --            print res
                                --     Left err ->
                                --         putStrLn ("Hoist Error:" ++ show err)
                         Left err ->
                             putStrLn $ "Lexical Error: " ++ show err
              Left err ->
                  putStrLn $ "Lexical Error: " ++ show err
