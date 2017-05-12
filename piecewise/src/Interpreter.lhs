> {-# LANGUAGE TupleSections #-}

> module Interpreter (interpret, interpretStmt) where
> import qualified Data.Text as T
> import Data.Text (Text)
> import Control.Monad.State
> import Control.Monad.Except
> import qualified AST (Expr(..), Const(..))
> import AST (Expr, Stmt(..))
> import qualified Env
> import Env (LexEnv, DynEnv, BindingError)
> import qualified Util

Value Representation
====================

> data Value = Int Int
>            | String Text

> instance Show Value where
>     show (Int i) = show i
>     show (String t) = T.unpack t

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

> type Interpreter a = StateT (Cont, ContDump) (ExceptT ItpError IO) a

> currentCont :: Interpreter Cont
> currentCont = gets fst

> currentDynEnv :: Interpreter (DynEnv Text Value)
> currentDynEnv = gets (\(Cont _ _ dEnv, _) -> dEnv)

> currentDump :: Interpreter ContDump
> currentDump = gets snd

Abstract Machine
================

> eval :: Expr -> LexEnv Text Value -> Interpreter Value
> eval (AST.Var _ name) env = lift (Env.lookup env name) >>= continue
> eval (AST.Const c) _ = continue (evalConst c)
>     where evalConst (AST.Int _ i) = Int i
>           evalConst (AST.String _ s) = String s

> continue :: Value -> Interpreter Value
> continue v = do k <- currentCont
>                 case k of
>                     Cont Halt _ _ -> return v

> -- apply :: Value -> [Value] -> Interpreter Value

> interpret :: LexEnv Text Value -> Expr -> IO (Either ItpError Value)
> interpret env expr = let action = eval expr env
>                      in do k <- haltCont
>                            runExceptT (evalStateT action (k, emptyDump))

> interpretStmt :: LexEnv Text Value -> Stmt -> IO (Either ItpError Value)
> interpretStmt env (Expr expr) = interpret env expr
