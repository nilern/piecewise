> {-# LANGUAGE TupleSections #-}

> module Interpreter (interpret, interpretStmt) where
> import Data.Text (Text)
> import Control.Monad.State
> import Control.Monad.Except
> import qualified Parsing.CST as CST (Expr(..), Const(..))
> import Parsing.CST (Expr, Stmt(..), Var(..))
> import qualified Interpreter.Env as Env
> import Interpreter.Env (BindingError)
> import qualified Interpreter.Cont as Cont
> import Interpreter.Cont (emptyDump)
> import qualified Util
> import Util (Name)

Value Representation
====================

> data Value = Int Int
>            | String Text

> instance Show Value where
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

> type Interpreter a = StateT (DynEnv, Cont, ContDump) (ExceptT ItpError IO) a

> currentDynEnv :: Interpreter DynEnv
> currentDynEnv = gets (\(e, _, _) -> e)

> setDynEnv :: DynEnv -> Interpreter ()
> setDynEnv dEnv = do (_, k, pks) <- get
>                     put (dEnv, k, pks)

> currentCont :: Interpreter Cont
> currentCont = gets (\(_, k, _) -> k)

> setCont :: Cont -> Interpreter ()
> setCont k = do (dEnv, _, pks) <- get
>                put (dEnv, k, pks)

> pushContFrame :: (Cont -> Cont) -> Interpreter ()
> pushContFrame makeCont = do k <- currentCont
>                             setCont (makeCont k)

> currentDump :: Interpreter ContDump
> currentDump = gets (\(_, _, pks) -> pks)

> evalInterpreter :: Interpreter Value -> DynEnv -> IO (Either ItpError Value)
> evalInterpreter m dEnv =
>     runExceptT (evalStateT m (dEnv, Cont.Halt, emptyDump))

Abstract Machine
================

> eval :: Expr -> LexEnv -> Interpreter Value
> eval (CST.Var (LexVar _ name)) env = lift (Env.lookup env name) >>= continue
> eval (CST.Var (DynVar _ name)) _ = do env <- currentDynEnv
>                                       lift (Env.lookup env name) >>= continue
> eval (CST.Const c) _ = continue (evalConst c)
>     where evalConst (CST.Int _ i) = Int i
>           evalConst (CST.String _ s) = String s

> evalStmt :: Stmt -> LexEnv -> Interpreter Value
> evalStmt (Def (CST.Var var) expr) lEnv =
>     do dEnv <- currentDynEnv
>        pushContFrame (case var of
>                           LexVar _ name ->
>                               (\k -> Cont.LexAssign k lEnv dEnv name)
>                           DynVar _ name ->
>                               (\k -> Cont.DynAssign k lEnv dEnv name))
>        eval expr lEnv
> evalStmt (Expr expr) env = eval expr env

> continue :: Value -> Interpreter Value
> continue v = do k <- currentCont
>                 case k of
>                     Cont.LexAssign k' lEnv dEnv name ->
>                         do lift (Env.insert lEnv name v)
>                            setCont k'
>                            setDynEnv dEnv
>                            continue v -- QUESTION: what to return here?
>                     Cont.DynAssign k' _ dEnv name ->
>                         do lift (Env.insert dEnv name v)
>                            setCont k'
>                            setDynEnv dEnv
>                            continue v -- QUESTION: what to return here?
>                     Cont.Halt -> return v

apply :: Value -> Value -> Interpreter Value
apply (Fn ...) (Tuple args) = ...

> interpret :: LexEnv -> DynEnv -> Expr -> IO (Either ItpError Value)
> interpret lEnv dEnv expr = evalInterpreter (eval expr lEnv) dEnv

> interpretStmt :: LexEnv -> DynEnv -> Stmt -> IO (Either ItpError Value)
> interpretStmt lEnv dEnv stmt = evalInterpreter (evalStmt stmt lEnv) dEnv
