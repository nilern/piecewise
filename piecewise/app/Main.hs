{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GADTs, RankNTypes #-}

module Main where
import Data.Default (def)
import Data.Foldable (traverse_)
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString as B
import Control.Eff (run)
import Control.Eff.Lift (lift, runLift)
import Control.Eff.Exception (throwExc, runExc)
import Control.Eff.State.Lazy (runState)

import Parsing.Parser (expr)
import Parsing.Lexer (Tok(..), TokTag(TokEOF), strToInput, LexicalError, Input)
import Parsing.Indentation (WSLexer, runWSLexer, readToken)
import qualified IR.AST as AST
import qualified IR.AST.Initial as IA
import qualified IR.AST.Hoisted as HA
import Pass.PatExpand (expandStmtList, runExpansion, PatError)
import Pass.HoistAugs (hoistedStmt, runHoisted, HoistError)
import Interpreter.Value (Value)
import Interpreter (Interpreter, ItpError(..), evalStmt, normalize, evalInterpreter)
import qualified Interpreter.Env as Env
import Util (Name, freshLabel)

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

act :: Input -> Either PwError ([HA.Stmt], [String])
act input =
    do tokens <- Bf.first PwParseError (runWSLexer (tokenize []) def input)
       let tokstrs = map show (reverse tokens)
       cstStmts <- Bf.first PwParseError (runWSLexer expr def input)
       (c, l) <- run $ runExc $ runState (0::Int) freshLabel
       (c', astStmts::[IA.Stmt]) <- Bf.first PwPatError
                                  (runExpansion (expandStmtList cstStmts) l c)
       (_, astStmts'::[HA.Stmt]) <- Bf.first PwHoistError
                                 (runHoisted c' (traverse hoistedStmt astStmts))
       return (astStmts',
               [concatMap (++ "\n") tokstrs,
                concatMap ((++ "\n") . show) cstStmts,
                concatMap ((++ "\n") . show) astStmts,
                concatMap ((++ "\n") . show) astStmts'])

actStmt :: LexEnv -> DynEnv -> HA.Stmt -> IO (Either ItpError Value)
actStmt lEnv dEnv stmt = evalInterpreter lEnv dEnv (evalStmt stmt >>= norm)

norm :: Value -> Interpreter Value
norm v = do ev <- lift $ runLift (runExc (normalize v))
            case ev of
                Right v -> pure v
                Left err -> throwExc (RedirectErr err)

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
