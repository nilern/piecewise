> module Interpreter (interpret) where
> import Data.Text (Text)
> import qualified Data.HashTable.IO as H
> import Control.Monad.Except
> import qualified AST (Expr(..), Const(..))
> import AST (Expr)

Value Representation
====================

> data Value = Int Int
>            | String Text

Errors
======

> data ItpError = Failure

Environments
============

Lexical Environment
-------------------

> data LexEnv = LexEnv LexEnv (H.BasicHashTable Text Value)
>             | GlobalEnv (H.BasicHashTable Text Value)

> emptyLexEnv :: IO LexEnv
> emptyLexEnv = GlobalEnv <$> H.new

Dynamic Environment
-------------------

> type DynEnv = [DynEnvFrame]
> data DynEnvFrame = DynEnvFrame (H.BasicHashTable Text Value)

> emptyDynEnv :: DynEnv
> emptyDynEnv = []

Continuations
=============

> data Cont = Cont CExpr LexEnv DynEnv

> haltCont :: IO Cont
> haltCont = Cont Halt <$> emptyLexEnv <*> pure emptyDynEnv

> data CExpr = Halt

Interpreter Monad
=================

> type Interpreter a = ExceptT ItpError IO a

Abstract Machine
================

> eval :: Expr -> LexEnv -> Cont -> Interpreter Value
> eval (AST.Const c) _ k = continue k (evalConst c)
>     where evalConst (AST.Int _ i) = Int i
>           evalConst (AST.String _ s) = String s

> continue :: Cont -> Value -> Interpreter Value
> continue (Cont Halt _ _) v = return v

> -- apply :: Value -> Cont -> [Value] -> Interpreter Value

> interpret :: Expr -> LexEnv -> IO (Either ItpError Value)
> interpret expr env = runExceptT (eval expr env =<< liftIO haltCont)
