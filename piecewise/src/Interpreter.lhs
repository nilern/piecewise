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
> import Interpreter.Cont (emptyDump)
> import qualified Util
> import Util (Name)

Value Representation
====================

> data Value = Closure [Method]
>            | Tuple [Value]
>            | Int Int
>            | String Text

> newtype Method = Method ([Name], Expr, LexEnv)

> method :: LexEnv -> ([Name], Expr) -> Method
> method lEnv (formals, body) = Method (formals, body, lEnv)

> instance Show Value where
>     show (Closure methods) = "#<Fn (" ++ show (length methods) ++ " methods)>"
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
>                 SetMember Lift (Lift IO) r)
>              => Eff r a

> type ItpState = (DynEnv, Cont, ContDump)

> currentDynEnv :: Interpreter DynEnv
> currentDynEnv = (\((e, _, _)::ItpState) -> e) <$> get

> setDynEnv :: DynEnv -> Interpreter ()
> setDynEnv dEnv = do (_, k, pks)::ItpState <- get
>                     put (dEnv, k, pks)

> currentCont :: Interpreter Cont
> currentCont = (\((_, k, _)::ItpState) -> k) <$> get

> setCont :: Cont -> Interpreter ()
> setCont k = do (dEnv, _, pks)::ItpState <- get
>                put (dEnv, k, pks)

> pushContFrame :: (Cont -> Cont) -> Interpreter ()
> pushContFrame makeCont = do k <- currentCont
>                             setCont (makeCont k)

> currentDump :: Interpreter ContDump
> currentDump = (\((_, _, pks)::ItpState) -> pks) <$> get

> evalInterpreter :: Interpreter Value -> DynEnv -> IO (Either ItpError Value)
> evalInterpreter m dEnv = runLift (runExc (evalState st m))
>     where st::ItpState = (dEnv, Cont.Halt, emptyDump)

Abstract Machine
================

> eval :: Expr -> LexEnv -> Interpreter Value
> eval (AST.Fn _ cases) lEnv = continue $ Closure (method lEnv <$> cases)
> eval (AST.Block _ (stmt:stmts)) lEnv =
>     do dEnv' <- lift . pushFrame =<< currentDynEnv
>        setDynEnv dEnv'
>        lEnv' <- lift (pushFrame lEnv)
>        pushContFrame (Cont.Stmt lEnv' dEnv' stmts)
>        evalStmt stmt lEnv'
> eval (AST.App _ f args) lEnv =
>     do dEnv <- currentDynEnv
>        pushContFrame (Cont.Applicant lEnv dEnv args)
>        eval f lEnv
> eval (AST.PrimApp _ op (arg:args)) lEnv =
>     do dEnv <- currentDynEnv
>        pushContFrame (Cont.PrimArg lEnv dEnv op [] args)
>        eval arg lEnv
> eval (AST.PrimApp _ op []) _ = applyPrimop op []
> eval (AST.Var (LexVar _ name)) env = Env.lookup env name >>= continue
> eval (AST.Var (GlobVar _ name)) env = Env.lookup env name >>= continue
> eval (AST.Var (DynVar _ name)) _ = do env <- currentDynEnv
>                                       Env.lookup env name >>= continue
> eval (AST.Const c) _ = continue (evalConst c)
>     where evalConst (CST.Int _ i) = Int i
>           evalConst (CST.String _ s) = String s

> evalStmt :: Stmt -> LexEnv -> Interpreter Value
> evalStmt (Def var expr) lEnv =
>     do dEnv <- currentDynEnv
>        pushContFrame (case var of
>                           GlobVar _ name -> Cont.LexAssign lEnv dEnv name
>                           LexVar _ name -> Cont.LexAssign lEnv dEnv name
>                           DynVar _ name -> Cont.DynAssign lEnv dEnv name)
>        eval expr lEnv
> evalStmt (Expr expr) env = eval expr env

> continue :: Value -> Interpreter Value
> continue v = do k <- currentCont
>                 case k of
>                     Cont.Stmt lEnv dEnv (stmt:stmts) k' ->
>                         do setCont (Cont.Stmt lEnv dEnv stmts k')
>                            setDynEnv dEnv
>                            evalStmt stmt lEnv
>                     Cont.Stmt lEnv dEnv [] k' ->
>                         do setCont k'
>                            setDynEnv dEnv
>                            continue v
>                     Cont.Applicant lEnv dEnv (arg:args) k' ->
>                         do setCont (Cont.Arg lEnv dEnv v [] args k')
>                            setDynEnv dEnv
>                            eval arg lEnv
>                     Cont.Applicant _ dEnv [] k' ->
>                         do setCont k'
>                            setDynEnv dEnv
>                            apply v []
>                     Cont.Arg lEnv dEnv f vs (arg:args) k' ->
>                         do setCont (Cont.Arg lEnv dEnv f (v:vs) args k')
>                            setDynEnv dEnv
>                            eval arg lEnv
>                     Cont.Arg _ dEnv f vs [] k' ->
>                         do setCont k'
>                            setDynEnv dEnv
>                            apply f (reverse (v:vs))
>                     Cont.PrimArg lEnv dEnv op vs (arg:args) k' ->
>                         do setCont (Cont.PrimArg lEnv dEnv op (v:vs) args k')
>                            setDynEnv dEnv
>                            eval arg lEnv
>                     Cont.PrimArg _ dEnv op vs [] k' ->
>                         do setCont k'
>                            setDynEnv dEnv
>                            applyPrimop op (reverse (v:vs))
>                     Cont.LexAssign lEnv dEnv name k' ->
>                         do Env.insert lEnv name v
>                            setCont k'
>                            setDynEnv dEnv
>                            continue v -- QUESTION: what to return here?
>                     Cont.DynAssign _ dEnv name k' ->
>                         do Env.insert dEnv name v
>                            setCont k'
>                            setDynEnv dEnv
>                            continue v -- QUESTION: what to return here?
>                     Cont.Halt -> return v

> apply :: Value -> [Value] -> Interpreter Value
> apply (Closure ((Method ([formal], body, lEnv)):_)) vs =
>     do dEnv' <- lift . pushFrame =<< currentDynEnv
>        setDynEnv dEnv'
>        lEnv' <- lift (pushFrame lEnv)
>        Env.insert lEnv' formal (Tuple vs)
>        eval body lEnv'

> applyPrimop :: Primop -> [Value] -> Interpreter Value
> applyPrimop _ _ = return (Int 0) -- TODO...

> interpret :: LexEnv -> DynEnv -> Expr -> IO (Either ItpError Value)
> interpret lEnv dEnv expr = evalInterpreter (eval expr lEnv) dEnv

> interpretStmt :: LexEnv -> DynEnv -> Stmt -> IO (Either ItpError Value)
> interpretStmt lEnv dEnv stmt = evalInterpreter (evalStmt stmt lEnv) dEnv
