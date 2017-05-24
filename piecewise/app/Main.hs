{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.Default (def)
import Data.Foldable (traverse_)
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString as B
import Parsing.Parser (expr)
import Parsing.Lexer (Tok(..), TokTag(TokEOF), strToInput, LexicalError, Input)
import Parsing.Indentation (WSLexer, runWSLexer, readToken)
import Parsing.CST (Stmt)
import qualified AST
import AST (Jump(..))
import PatExpand (expandStmtList, runExpansion, PatError)
import Alphatize (alphatizeStmt, runAlphatization)
import Interpreter (interpretStmt)
import Interpreter.Env (emptyLexEnv, emptyDynEnv)

data PwError = PwParseError LexicalError
             | PwPatError PatError
             deriving Show

tokenize :: [Tok] -> WSLexer [Tok]
tokenize toks = do tok <- readToken
                   case tok of
                       Tok TokEOF _ _ _ -> return (tok : toks)
                       Tok _ _ _ _ -> tokenize (tok : toks)

act :: Input -> Either PwError ([AST.Stmt], [String])
act input =
    do tokens <- Bf.first PwParseError (runWSLexer (tokenize []) def input)
       let tokstrs = map show (reverse tokens)
       cstStmts <- Bf.first PwParseError (runWSLexer expr def input)
       let c = 0
       (c', astStmts::[AST.Stmt]) <- Bf.first PwPatError
                                  (runExpansion (expandStmtList cstStmts)
                                                ThrowBindErr c)
       let (_, alphStmts::[AST.Stmt]) = runAlphatization
                                            (traverse alphatizeStmt astStmts) c'
       return (alphStmts,
               [concatMap (++ "\n") tokstrs,
                concatMap ((++ "\n") . show) cstStmts,
                concatMap ((++ "\n") . show) astStmts,
                concatMap ((++ "\n") . show) alphStmts])

main :: IO ()
main = do input <- strToInput <$> B.getContents
          case act input of
              Right (stmts, output) ->
                  do traverse_ putStrLn output
                     print stmts
                     lEnv <- emptyLexEnv
                     dEnv <- emptyDynEnv
                     value <- traverse (interpretStmt lEnv dEnv) stmts
                     print value
              Left err -> putStrLn ("Compilation Error: " ++ show err)
