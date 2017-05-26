> {-# LANGUAGE RankNTypes, GADTs #-}
> {-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TupleSections #-}

> module Interpreter (interpret, interpretStmt) where
> import Data.List (intercalate)
> import Data.Text (Text)
> import Control.Eff
> import Control.Eff.Lift
> import Control.Eff.State.Lazy
> import Control.Eff.Exception
> import qualified Parsing.CST as CST (Const(..))
> import Parsing.CST (Var(..))
> import qualified AST (Expr(..))
> import AST (Expr, Stmt(..))
> import Ops (Primop)
> import qualified Interpreter.Env as Env
> import Interpreter.Env (pushFrame, BindingError)
> import qualified Interpreter.Cont as Cont
> import Interpreter.Cont (emptyDump, frames)
> import qualified Util
> import Util (Name)

Value Representation
====================

> data Value = Closure [Method]
>            | Tuple [Value]
>            | Int Int
>            | String Text

> newtype Method = Method ([Var], Expr, LexEnv)

> method :: LexEnv -> ([Var], Expr) -> Method
> method lEnv (formals, body) = Method (formals, body, lEnv)

> instance Show Value where
>     show (Closure methods) = "#<Fn (" ++ show (length methods) ++ " methods)>"
>     show (Tuple [v]) = '(' : show v ++ ",)"
>     show (Tuple vs) = '(' : intercalate ", " (map show vs) ++ ")"
>     show (Int i) = show i
>     show (String t) = show t

Errors
======

> type ItpError = Util.ItpError (BindingError Name)

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

> evalInterpreter :: Interpreter Value -> LexEnv -> DynEnv
>                 -> IO (Either ItpError Value)
> evalInterpreter m lEnv dEnv =
>     runLift (runExc (evalState lEnv (evalState st m)))
>     where st::ItpState = (dEnv, Cont.Halt, emptyDump)

> getLexEnv :: (Member (State LexEnv) r) => Eff r LexEnv
> getLexEnv = get

> putLexEnv :: (Member (State LexEnv) r) => LexEnv -> Eff r ()
> putLexEnv = put

> lookupLex :: (Member (Exc ItpError) r,
>               Member (State LexEnv) r, SetMember Lift (Lift IO) r)
>           => Name -> Eff r Value
> lookupLex name = do lEnv <- getLexEnv
>                     Env.lookup lEnv name

> defLex :: (Member (Exc ItpError) r,
>            Member (State LexEnv) r, SetMember Lift (Lift IO) r)
>        => Name -> Value -> Eff r ()
> defLex name val = do lEnv <- getLexEnv
>                      Env.insert lEnv name val

> getDynEnv :: (Member (State ItpState) r) => Eff r DynEnv
> getDynEnv = (\((e, _, _)::ItpState) -> e) <$> get

> putDynEnv :: (Member (State ItpState) r) => DynEnv -> Eff r ()
> putDynEnv dEnv = do (_, k, pks)::ItpState <- get
>                     put (dEnv, k, pks)

> lookupDyn :: (Member (Exc ItpError) r,
>               Member (State ItpState) r, SetMember Lift (Lift IO) r)
>           => Name -> Eff r Value
> lookupDyn name = do dEnv <- getDynEnv
>                     Env.lookup dEnv name

> defDyn :: (Member (Exc ItpError) r,
>            Member (State ItpState) r, SetMember Lift (Lift IO) r)
>        => Name -> Value -> Eff r ()
> defDyn name val = do dEnv <- getDynEnv
>                      Env.insert dEnv name val

> def :: (Member (Exc ItpError) r,
>         Member (State ItpState) r, Member (State LexEnv) r,
>         SetMember Lift (Lift IO) r)
>     => Var -> Value -> Eff r ()
> def (LexVar _ name) val = defLex name val
> def (GlobVar _ name) val = defLex name val
> def (DynVar _ name) val = defDyn name val

> pushScope :: (Member (Exc ItpError) r,
>               Member (State ItpState) r, Member (State LexEnv) r,
>               SetMember Lift (Lift IO) r)
>           => LexEnv -> Eff r ()
> pushScope lEnv = do dEnv' <- lift . pushFrame =<< getDynEnv
>                     lEnv' <- lift (pushFrame lEnv)
>                     putDynEnv dEnv'
>                     putLexEnv lEnv'

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
>                      Nothing -> throwExc (Util.StackUnderflow::ItpError)

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
> eval (AST.Block _ ([stmt])) =
>     do pushScope =<< getLexEnv
>        evalStmt stmt
> eval (AST.Block _ (stmt:stmts)) =
>     do pushScope =<< getLexEnv
>        pushContFrame (Cont.Stmt stmts)
>        evalStmt stmt
> eval (AST.App _ f args) =
>     do pushContFrame (Cont.Applicant args)
>        eval f
> eval (AST.PrimApp _ op (arg:args)) =
>     do pushContFrame (Cont.PrimArg op [] args)
>        eval arg
> eval (AST.PrimApp _ op []) = applyPrimop op []
> eval (AST.Var (LexVar _ name)) = continue =<< lookupLex name
> eval (AST.Var (GlobVar _ name)) = continue =<< lookupLex name
> eval (AST.Var (DynVar _ name)) = continue =<< lookupDyn name
> eval (AST.Const c) = continue (evalConst c)
>     where evalConst (CST.Int _ i) = Int i
>           evalConst (CST.String _ s) = String s

> evalStmt :: Stmt -> Interpreter Value
> evalStmt (Def var expr) =
>     do pushContFrame (case var of
>                           GlobVar _ name -> Cont.LexAssign name
>                           LexVar _ name -> Cont.LexAssign name
>                           DynVar _ name -> Cont.DynAssign name)
>        eval expr
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
>                     Cont.LexAssign name _ _ _ ->
>                         do pop
>                            defLex name v
>                            continue v -- QUESTION: what to return here?
>                     Cont.DynAssign name _ _ _ ->
>                         do pop
>                            defDyn name v
>                            continue v -- QUESTION: what to return here?
>                     Cont.Halt -> return v

> apply :: Value -> [Value] -> Interpreter Value
> apply (Closure ((Method ([formal], body, lEnv)):_)) vs =
>     do pushScope lEnv
>        def formal (Tuple vs)
>        eval body

> applyPrimop :: Primop -> [Value] -> Interpreter Value
> applyPrimop _ _ = return (Int 0) -- TODO...

> interpret :: LexEnv -> DynEnv -> Expr -> IO (Either ItpError Value)
> interpret lEnv dEnv expr = evalInterpreter (eval expr) lEnv dEnv

> interpretStmt :: LexEnv -> DynEnv -> Stmt -> IO (Either ItpError Value)
> interpretStmt lEnv dEnv stmt = evalInterpreter (evalStmt stmt) lEnv dEnv
