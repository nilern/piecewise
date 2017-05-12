> {-# LANGUAGE TupleSections #-}

> module Interpreter (interpret) where
> import Data.Text (Text)
> import qualified Data.HashTable.IO as H
> import Control.Monad.Except
> import qualified AST (Expr(..), Const(..))
> import AST (Expr)
> import qualified Env
> import Env (LexEnv, DynEnv, BindingError)
> import qualified Util

Value Representation
====================

> data Value = Int Int
>            | String Text

Errors
======

> type ItpError = Util.ItpError (BindingError Text)

Continuations
=============

> data Cont = Cont CExpr (LexEnv Text Value) (DynEnv Text Value)

> haltCont :: IO Cont
> haltCont = Cont Halt <$> Env.emptyLexEnv <*> Env.emptyDynEnv

> data CExpr = Halt

> type Prompt = Int
> data ContDump = CDump [(Prompt, Cont)]

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

> type Interpreter a = ExceptT ItpError IO a

Abstract Machine
================

> eval :: Expr -> LexEnv Text Value -> Cont -> Interpreter Value
> eval (AST.Var _ name) env k = Env.lookup env name >>= continue k
> eval (AST.Const c) _ k = continue k (evalConst c)
>     where evalConst (AST.Int _ i) = Int i
>           evalConst (AST.String _ s) = String s

> continue :: Cont -> Value -> Interpreter Value
> continue (Cont Halt _ _) v = return v

> -- apply :: Value -> Cont -> [Value] -> Interpreter Value

> interpret :: Expr -> LexEnv Text Value
>                   -> IO (Either ItpError Value, LexEnv Text Value)
> interpret expr env = do res <- runExceptT (eval expr env =<< liftIO haltCont)
>                         return (res, env)
