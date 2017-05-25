> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}

> module Alphatize (alphatize, alphatizeStmt, runAlphatization) where
> import Prelude hiding (lookup)
> import Data.Maybe (fromMaybe)
> import qualified Data.Map as Map
> import Data.Map (Map)
> import Control.Eff
> import Control.Eff.State.Lazy
> import Control.Eff.Reader.Lazy

> import Parsing.CST (Var(..))
> import AST (Expr(..), Stmt(..))
> import Util (Name(..), nameChars, Pos)

> type Alphatization a =
>     forall r . (Member (Reader Env) r, Member (State Int) r)
>              => Eff r a

> runAlphatization :: Alphatization a -> Int -> (Int, a)
> runAlphatization alph counter =
>     run (runReader (runState counter alph) emptyEnv)

> stmtListBounds :: [Stmt] -> [Name]
> stmtListBounds stmts = stmts >>= stmtBounds

> stmtBounds :: Stmt -> [Name]
> stmtBounds (Def (LexVar _ name) _) = [name]
> stmtBounds (AugDef (LexVar _ name) _) = [name]
> stmtBounds (Expr _) = []

> type Env = [Map Name Var]

> emptyEnv :: Env
> emptyEnv = []

> lookup :: Pos -> Name -> Alphatization Var
> lookup pos name = find <$> ask
>     where find (kvs:p) = fromMaybe (find p) (Map.lookup name kvs)
>           find [] = GlobVar pos name

FIXME: [Var] -> Env -> Alphatization (Env, [Name]) as soon as AST.Fn gets fixed

> pushFrame :: Pos -> [Name] -> Env -> Alphatization (Env, [Name])
> pushFrame pos names env =
>     do names' <- traverse rename names
>        let bindings = Map.fromList $ zip names (LexVar pos <$> names')
>        return (bindings : env, names')

> rename :: Name -> Alphatization Name
> rename (nameChars -> name) = do res <- UniqueName name <$> get
>                                 modify (+ (1::Int))
>                                 return res

> alphatize :: Expr -> Alphatization Expr
> alphatize (Fn pos cases) = Fn pos <$> traverse alphatizeCase cases
>     where alphatizeCase (args, body) =
>                do (env', args') <- pushFrame pos args =<< ask
>                   body' <- local (const env') (alphatize body)
>                   pure (args', body')
> alphatize (Block pos stmts) =
>     do (env', _) <- pushFrame pos (stmtListBounds stmts) =<< ask
>        Block pos <$> local (const env') (traverse alphatizeStmt stmts)
> alphatize (App pos f args) =
>     App pos <$> alphatize f <*> traverse alphatize args
> alphatize (PrimApp pos op args) = PrimApp pos op <$> traverse alphatize args
> alphatize (Var (LexVar pos name)) = Var <$> lookup pos name
> alphatize c @ (Const _) = pure c

> alphatizeStmt :: Stmt -> Alphatization Stmt
> alphatizeStmt (Def v val) = Def <$> alphatizeVar v <*> alphatize val
> alphatizeStmt (AugDef v val) = AugDef <$> alphatizeVar v <*> alphatize val
> alphatizeStmt (Expr expr) = Expr <$> alphatize expr

> alphatizeVar :: Var -> Alphatization Var
> alphatizeVar (LexVar pos name) = lookup pos name
> alphatizeVar v @ (DynVar _ _) = return v
