> {-# LANGUAGE ViewPatterns #-}

> module Alphatize (alphatize, alphatizeStmt, runAlphatization, AlphError) where
> import Prelude hiding (lookup)
> import Data.Maybe (fromMaybe)
> import qualified Data.Map as Map
> import Data.Map (Map)
> import Control.Monad.State
> import Control.Monad.Except
> import Control.Monad.Reader

> import Parsing.CST (Expr(..), Stmt(..), Var(..))
> import Util (Name(..), nameChars, Pos)

> data AlphError = InvalidPattern Expr deriving Show

> patVars :: Expr -> Either AlphError [Var]
> patVars pat @ (Fn _ _) = throwError $ InvalidPattern pat
> patVars pat @ (Block _ _) = throwError  $ InvalidPattern pat
> patVars (App _ _ pats) = patListVars pats
> patVars (PrimApp _ _ pats) = patListVars pats
> patVars (Var var) = pure [var]
> patVars (Const _) = pure []

> patListVars :: [Expr] -> Either AlphError [Var]
> patListVars = foldM (\acc pat -> mappend acc <$> patVars pat) mempty

> stmtListVars :: [Stmt] -> Either AlphError [Var]
> stmtListVars = foldM (\acc stmt -> mappend acc <$> stmtVars stmt) mempty
>     where stmtVars (Def pat _) = patVars pat
>           stmtVars (AugDef pat _) = patVars pat
>           stmtVars (Expr _) = pure []

> type Alphatization a = ReaderT Env (StateT Int (Either AlphError)) a

> runAlphatization :: Alphatization a -> Int -> Either AlphError (a, Int)
> runAlphatization alph counter = runStateT (runReaderT alph []) counter

> type Env = [Map Name Var]

TODO: Parameterize the code to use either a magical (global hashtable) or
      a consistent (regular block) toplevel.

> lookup :: Pos -> Name -> Alphatization Var
> lookup pos name = asks find
>     where find (kvs:p) = fromMaybe (find p) (Map.lookup name kvs)
>           find [] = GlobVar pos name

> pushFrame :: [Var] -> Env -> Alphatization Env
> pushFrame names env = do kvs <- Map.fromList <$> traverse renaming names
>                          return (kvs : env)
>     where renaming (LexVar pos name) = do name' <- rename name
>                                           pure (name, LexVar pos name')

> rename :: Name -> Alphatization Name
> rename (nameChars -> name) = do res <- gets (UniqueName name)
>                                 modify (+ 1)
>                                 return res

> alphatize :: Expr -> Alphatization Expr
> alphatize (Fn pos cases) = Fn pos <$> traverse alphatizeCase cases
>     where alphatizeCase (pats, cond, body) =
>                do vars <- lift $ lift $ patListVars pats
>                   env' <- pushFrame vars =<< ask
>                   pats' <- local (const env') (traverse alphatizePat pats)
>                   cond' <- local (const env') (alphatize cond)
>                   body' <- local (const env') (alphatize body)
>                   pure (pats', cond', body)
> alphatize (Block pos stmts) =
>     do vars <- lift $ lift $ stmtListVars stmts
>        env' <- pushFrame vars =<< ask
>        Block pos <$> local (const env') (traverse alphatizeStmt stmts)
> alphatize (App pos f args) =
>     App pos <$> alphatize f <*> traverse alphatize args
> alphatize (PrimApp pos op args) = PrimApp pos op <$> traverse alphatize args
> alphatize (Var (LexVar pos name)) = Var <$> lookup pos name
> alphatize c @ (Const _) = pure c

> alphatizeStmt :: Stmt -> Alphatization Stmt
> alphatizeStmt (Def pat val) = Def <$> alphatizePat pat <*> alphatize val
> alphatizeStmt (AugDef pat val) = AugDef <$> alphatizePat pat <*> alphatize val
> alphatizeStmt (Expr expr) = Expr <$> alphatize expr

> alphatizePat :: Expr -> Alphatization Expr
> alphatizePat pat @ (Fn _ _) = throwError  $ InvalidPattern pat
> alphatizePat pat @ (Block _ _) = throwError  $ InvalidPattern pat
> alphatizePat (App pos f args) =
>     App pos <$> alphatizePat f <*> traverse alphatizePat args
> alphatizePat (PrimApp pos op args) =
>     PrimApp pos op <$> traverse alphatizePat args
> alphatizePat (Var (LexVar pos name)) = Var <$> lookup pos name
> alphatizePat c @ (Const _) = pure c
