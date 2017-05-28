{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.Default (def)
import Data.Foldable (traverse_)
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString as B

import Parsing.Parser (expr)
import Parsing.Lexer (Tok(..), TokTag(TokEOF), strToInput, LexicalError, Input)
import Parsing.Indentation (WSLexer, runWSLexer, readToken)
import qualified IR.AST as AST
import IR.AST (Stmt, Jump(..))
import Pass.PatExpand (expandStmtList, runExpansion, PatError)
import Pass.HoistAugs (hoistedStmt, runHoisted, HoistError)
import Interpreter (Value, ItpError, evalStmt, normalize, evalInterpreter)
import qualified Interpreter.Env as Env
import Util (Name)

type LexEnv = Env.LexEnv Name Value
type DynEnv = Env.DynEnv Name Value

data PwError = PwParseError LexicalError
             | PwPatError PatError
             | PwHoistError HoistError
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
       (_, astStmts'::[AST.Stmt]) <- Bf.first PwHoistError
                                 (runHoisted c' (traverse hoistedStmt astStmts))
       return (astStmts,
               [concatMap (++ "\n") tokstrs,
                concatMap ((++ "\n") . show) cstStmts,
                concatMap ((++ "\n") . show) astStmts,
                concatMap ((++ "\n") . show) astStmts'])

actStmt :: LexEnv -> DynEnv -> Stmt -> IO (Either ItpError Value)
actStmt lEnv dEnv stmt = evalInterpreter lEnv dEnv (evalStmt stmt >>= normalize)

main :: IO ()
main = do input <- strToInput <$> B.getContents
          case act input of
              Right (stmts, output) ->
                  do traverse_ putStrLn output
                     lEnv::Env.LexEnv Name Value <- Env.toplevel
                     dEnv::Env.DynEnv Name Value <- Env.toplevel
                     vs <- sequence (map (actStmt lEnv dEnv) stmts)
                     print vs
              Left err -> putStrLn ("Compilation Error: " ++ show err)
