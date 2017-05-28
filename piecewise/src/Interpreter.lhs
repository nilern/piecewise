> {-# LANGUAGE RankNTypes, GADTs #-}
> {-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TupleSections #-}

> module Interpreter (Value, ItpError,
>                     eval, evalStmt, unwrap, normalize, evalInterpreter) where
> import Prelude hiding (lookup)
> import Data.IORef (IORef, newIORef, readIORef, writeIORef)
> import Data.List (intercalate)
> import Data.Text (Text)
> import Control.Eff
> import Control.Eff.Lift
> import Control.Eff.State.Lazy
> import Control.Eff.Exception
> import qualified Parsing.CST as CST (Const(..))
> import Parsing.CST (Var(..), varName, isLexVar, isDynVar)
> import qualified AST (Expr(..))
> import AST (Expr, Stmt(..), stmtBinders)
> import Ops (Primop)
> import qualified Interpreter.Env as Env
> import Interpreter.Env (pushFrame)
> import qualified Interpreter.Cont as Cont
> import Interpreter.Cont (emptyDump, frames)
> import Util (Name, Pos)

Value Representation
====================

> data Value = Closure [Method]
>            | Tuple [Value]
>            | Int Int
>            | String Text
>            | Redirect (IORef (Maybe Value))

> newtype Method = Method ([Var], Expr, LexEnv)

> method :: LexEnv -> ([Var], Expr) -> Method
> method lEnv (formals, body) = Method (formals, body, lEnv)

> emptyRedirect :: IO Value
> emptyRedirect = Redirect <$> newIORef Nothing

> unwrap :: Value -> Interpreter Value
> unwrap (Redirect ref) = do ov <- lift $ readIORef ref
>                            case ov of
>                                Just v -> unwrap v
>                                Nothing -> throwExc UnAssigned
> unwrap v = return v

> normalize :: Value -> Interpreter Value
> normalize f @ (Closure _) = pure f
> normalize (Tuple vs) = Tuple <$> traverse normalize vs
> normalize i @ (Int _) = pure i
> normalize s @ (String _) = pure s
> normalize r @ (Redirect _) = normalize =<< unwrap r

> instance Show Value where
>     show (Closure methods) = "#<Fn (" ++ show (length methods) ++ " methods)>"
>     show (Tuple [v]) = '(' : show v ++ ",)"
>     show (Tuple vs) = '(' : intercalate ", " (map show vs) ++ ")"
>     show (Int i) = show i
>     show (String t) = show t
>     show (Redirect _) = "#<Redirect>"

Errors
======

> data ItpError = Unbound Pos Name
>               | UnAssigned
>               | ReAssignment Pos Name
>               | UnAssignable Pos Name
>               | StackUnderflow
>               deriving Show

Environments
============

> type LexEnv = Env.LexEnv Name Value
> type DynEnv = Env.DynEnv Name Value

Continuations
=============

> type Cont = Cont.Cont Name Value
> type ContDump = Cont.ContDump Name Value

Interpreter Monad
=================

> type Interpreter a =
>     forall r . (Member (Exc ItpError) r,
>                 Member (State ItpState) r,
>                 Member (State LexEnv) r,
>                 SetMember Lift (Lift IO) r)
>              => Eff r a

> type ItpState = (DynEnv, Cont, ContDump)

> evalInterpreter :: LexEnv -> DynEnv -> Interpreter a
>                 -> IO (Either ItpError a)
> evalInterpreter lEnv dEnv m =
>     runLift (runExc (evalState lEnv (evalState st m)))
>     where st::ItpState = (dEnv, Cont.Halt, emptyDump)

> getLexEnv :: (Member (State LexEnv) r) => Eff r LexEnv
> getLexEnv = get

> putLexEnv :: (Member (State LexEnv) r) => LexEnv -> Eff r ()
> putLexEnv = put

> getDynEnv :: (Member (State ItpState) r) => Eff r DynEnv
> getDynEnv = (\((e, _, _)::ItpState) -> e) <$> get

> putDynEnv :: (Member (State ItpState) r) => DynEnv -> Eff r ()
> putDynEnv dEnv = do (_, k, pks)::ItpState <- get
>                     put (dEnv, k, pks)

> lookup :: Var -> Interpreter Value
> lookup var =
>     case var of
>         LexVar pos name ->
>             do lEnv <- getLexEnv
>                dflt <- lift emptyRedirect
>                ov <- lift $ Env.lookup lEnv name dflt
>                case ov of
>                    Just v -> return v
>                    Nothing -> throwExc (Unbound pos name)
>         DynVar pos name ->
>             do dEnv <- getDynEnv
>                dflt <- lift emptyRedirect
>                ov <- lift $ Env.lookup dEnv name dflt
>                case ov of
>                    Just v -> return v
>                    Nothing -> throwExc (Unbound pos name)

> def :: Var -> Value -> Interpreter ()
> def var val =
>     case var of
>         LexVar pos name ->
>             do lEnv <- getLexEnv
>                dflt <- lift emptyRedirect
>                orv <- lift $ Env.lookup lEnv name dflt
>                case orv of
>                    Just (Redirect ref) ->
>                        do ov <- lift $ readIORef ref
>                           case ov of
>                               Nothing -> lift (writeIORef ref (Just val))
>                               Just _ -> throwExc (ReAssignment pos name)
>                    _ -> throwExc (UnAssignable pos name)
>         DynVar pos name ->
>             do dEnv <- getDynEnv
>                dflt <- lift emptyRedirect
>                orv <- lift $ Env.lookup dEnv name dflt
>                case orv of
>                    Just (Redirect ref) ->
>                        do ov <- lift $ readIORef ref
>                           case ov of
>                               Nothing -> lift (writeIORef ref (Just val))
>                               Just _ -> throwExc (ReAssignment pos name)
>                    _ -> throwExc (UnAssignable pos name)

> pushScope :: (Member (Exc ItpError) r,
>               Member (State ItpState) r, Member (State LexEnv) r,
>               SetMember Lift (Lift IO) r)
>           => LexEnv -> [(Name, Value)] -> [(Name, Value)] -> Eff r ()
> pushScope lEnv lkvs dkvs = do dEnv' <- pushFrame <$> getDynEnv <*> pure dkvs
>                               let lEnv' = pushFrame lEnv lkvs
>                               putDynEnv dEnv'
>                               putLexEnv lEnv'

> getCont :: (Member (State ItpState) r) => Eff r Cont
> getCont = (\((_, k, _)::ItpState) -> k) <$> get

> putCont :: (Member (State ItpState) r) => Cont -> Eff r ()
> putCont k = do (dEnv, _, pks)::ItpState <- get
>                put (dEnv, k, pks)

> pushContFrame :: (Member (State ItpState) r, Member (State LexEnv) r)
>               => (LexEnv -> DynEnv -> Cont -> Cont) -> Eff r ()
> pushContFrame makeCont = do lEnv <- getLexEnv
>                             dEnv <- getDynEnv
>                             k <- getCont
>                             putCont (makeCont lEnv dEnv k)

> getDump :: (Member (State ItpState) r) => Eff r ContDump
> getDump = (\((_, _, pks)::ItpState) -> pks) <$> get

> modifyTop :: (Member (Exc ItpError) r,
>               Member (State ItpState) r, Member (State LexEnv) r)
>           => (LexEnv -> DynEnv -> Cont -> Cont) -> Eff r ()
> modifyTop f = do k <- getCont
>                  case frames k of
>                      Just (lEnv, dEnv, k') ->
>                          do putCont (f lEnv dEnv k')
>                             putLexEnv lEnv
>                             putDynEnv dEnv
>                      Nothing -> throwExc (StackUnderflow::ItpError)

> pop :: (Member (Exc ItpError) r,
>         Member (State ItpState) r, Member (State LexEnv) r)
>     => Eff r ()
> pop = modifyTop (\_ _ k -> k)

Abstract Machine
================

> eval :: Expr -> Interpreter Value
> eval (AST.Fn _ cases) =
>     do lEnv <- getLexEnv
>        continue $ Closure (method lEnv <$> cases)
> eval (AST.Block _ stmts) =
>     do lEnv <- getLexEnv
>        let vars = stmtBinders =<< stmts
>        let lVars = varName <$> filter isLexVar vars
>        let dVars = varName <$> filter isDynVar vars
>        lVals <- lift $ sequenceA (replicate (length lVars) emptyRedirect)
>        dVals <- lift $ sequenceA (replicate (length dVars) emptyRedirect)
>        pushScope lEnv (zip lVars lVals) (zip dVars dVals)
>        case stmts of
>            [stmt] -> evalStmt stmt
>            stmt:stmts' -> do pushContFrame (Cont.Stmt stmts')
>                              evalStmt stmt
> eval (AST.App _ f args) =
>     do pushContFrame (Cont.Applicant args)
>        eval f
> eval (AST.PrimApp _ op (arg:args)) =
>     do pushContFrame (Cont.PrimArg op [] args)
>        eval arg
> eval (AST.PrimApp _ op []) = applyPrimop op []
> eval (AST.Var var) = continue =<< lookup var
> eval (AST.Const c) = continue (evalConst c)
>     where evalConst (CST.Int _ i) = Int i
>           evalConst (CST.String _ s) = String s

> evalStmt :: Stmt -> Interpreter Value
> evalStmt (Def var expr) = do pushContFrame (Cont.Assign var)
>                              eval expr
> evalStmt (Expr expr) = eval expr

> continue :: Value -> Interpreter Value
> continue v = do k <- getCont
>                 case k of
>                     Cont.Stmt [stmt] _ _ _ ->
>                         do pop
>                            evalStmt stmt
>                     Cont.Stmt (stmt:stmts) _ _ _ ->
>                         do modifyTop (Cont.Stmt stmts)
>                            evalStmt stmt
>                     Cont.Applicant (arg:args) _ _ _ ->
>                         do modifyTop (Cont.Arg v [] args)
>                            eval arg
>                     Cont.Applicant [] _ _ _ ->
>                         do pop
>                            apply v []
>                     Cont.Arg f vs (arg:args) _ _ _ ->
>                         do modifyTop (Cont.Arg f (v:vs) args)
>                            eval arg
>                     Cont.Arg f vs [] _ _ _ ->
>                         do pop
>                            apply f (reverse (v:vs))
>                     Cont.PrimArg op vs (arg:args) _ _ _ ->
>                         do modifyTop (Cont.PrimArg op (v:vs) args)
>                            eval arg
>                     Cont.PrimArg op vs [] _ _ _ ->
>                         do pop
>                            applyPrimop op (reverse (v:vs))
>                     Cont.Assign var _ _ _ ->
>                         do pop
>                            def var v
>                            continue v -- QUESTION: what to return here?
>                     Cont.Halt -> return v

> apply :: Value -> [Value] -> Interpreter Value
> apply f args = unwrap f >>= flip applyDirect args

> applyDirect :: Value -> [Value] -> Interpreter Value
> applyDirect (Closure ((Method ([LexVar _ fname], body, lEnv)):_)) vs =
>     do pushScope lEnv [(fname, Tuple vs)] []
>        eval body

> applyPrimop :: Primop -> [Value] -> Interpreter Value
> applyPrimop _ _ = return (Int 0) -- TODO...
