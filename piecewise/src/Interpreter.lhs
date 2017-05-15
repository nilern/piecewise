> {-# LANGUAGE TupleSections #-}

> module Interpreter (interpret, interpretStmt) where
> import Data.Text (Text)
> import Control.Monad.State
> import Control.Monad.Except
> import qualified AST (Expr(..), Const(..))
> import AST (Expr, Stmt(..), Var(..))
> import qualified Env
> import Env (BindingError, emptyDynEnv)
> import qualified Util

Value Representation
====================

> data Value = Int Int
>            | String Text

> instance Show Value where
>     show (Int i) = show i
>     show (String t) = show t

Errors
======

> type ItpError = Util.ItpError (BindingError Text)

Environments
============

> type LexEnv = Env.LexEnv Text Value
> type DynEnv = Env.DynEnv Text Value

Continuations
=============

> data Cont = Cont (Maybe Cont) CExpr LexEnv DynEnv

> haltCont :: IO Cont
> haltCont = Cont Nothing Halt <$> Env.emptyLexEnv <*> Env.emptyDynEnv

> data CExpr = LexAssign Text
>            | DynAssign Text
>            | Halt

> type Prompt = Int
> data ContDump = CDump [(Prompt, Cont)]

> emptyDump :: ContDump
> emptyDump = CDump []

> pushCont :: ContDump -> Prompt -> Cont -> ContDump
> pushCont (CDump pks) p k = CDump ((p, k) : pks)

> popCont :: ContDump -> Maybe (Cont, ContDump)
> popCont (CDump ((_, k):pks)) = Just (k, CDump pks)
> popCont (CDump []) = Nothing

> splitDump :: ContDump -> Prompt -> Maybe (ContDump, ContDump)
> splitDump (CDump pks) p =
>     case break ((== p) . fst) pks of
>         (_, []) -> Nothing
>         (pks', pks'') -> Just (CDump pks', CDump pks'')

Interpreter Monad
=================

> type Interpreter a = StateT (DynEnv, Cont, ContDump) (ExceptT ItpError IO) a

> currentDynEnv :: Interpreter DynEnv
> currentDynEnv = gets (\(e, _, _) -> e)

> currentCont :: Interpreter Cont
> currentCont = gets (\(_, k, _) -> k)

> setCont :: Cont -> Interpreter ()
> setCont k = do (dEnv, _, pks) <- get
>                put (dEnv, k, pks)

> pushContFrame :: (Cont -> Cont) -> Interpreter ()
> pushContFrame makeCont = do k <- currentCont
>                             setCont (makeCont k)

> popContFrame :: Interpreter ()
> popContFrame = do (_, (Cont (Just k) _ _ dEnv), pks) <- get
>                   put (dEnv, k, pks)

> currentDump :: Interpreter ContDump
> currentDump = gets (\(_, _, pks) -> pks)

> evalInterpreter :: Interpreter Value -> DynEnv -> IO (Either ItpError Value)
> evalInterpreter m dEnv = do k <- haltCont
>                             runExceptT (evalStateT m (dEnv, k, emptyDump))

Abstract Machine
================

> eval :: Expr -> LexEnv -> Interpreter Value
> eval (AST.Var (LexVar _ name)) env = lift (Env.lookup env name) >>= continue
> eval (AST.Var (DynVar _ name)) _ = do env <- currentDynEnv
>                                       lift (Env.lookup env name) >>= continue
> eval (AST.Const c) _ = continue (evalConst c)
>     where evalConst (AST.Int _ i) = Int i
>           evalConst (AST.String _ s) = String s

> evalStmt :: Stmt -> LexEnv -> Interpreter Value
> evalStmt (Def (AST.Var var) expr) env =
>     do dEnv <- currentDynEnv
>        pushContFrame (\k -> (Cont (Just k) cexpr env dEnv))
>        eval expr env
>     where cexpr = case var of
>                       LexVar _ name -> LexAssign name
>                       DynVar _ name -> DynAssign name
> evalStmt (Expr expr) env = eval expr env

> continue :: Value -> Interpreter Value
> continue v = do k <- currentCont
>                 case k of
>                     Cont _ (LexAssign name) lEnv _ ->
>                         do lift (Env.insert lEnv name v)
>                            popContFrame
>                            continue v -- QUESTION: what to return here?
>                     Cont _ (DynAssign name) _ dEnv ->
>                         do lift (Env.insert dEnv name v)
>                            popContFrame
>                            continue v -- QUESTION: what to return here?
>                     Cont _ Halt _ _ -> return v

apply :: Value -> Value -> Interpreter Value
apply (Fn ...) (Tuple args) = ...

> interpret :: LexEnv -> DynEnv -> Expr -> IO (Either ItpError Value)
> interpret lEnv dEnv expr = evalInterpreter (eval expr lEnv) dEnv

> interpretStmt :: LexEnv -> DynEnv -> Stmt -> IO (Either ItpError Value)
> interpretStmt lEnv dEnv stmt = evalInterpreter (evalStmt stmt lEnv) dEnv
