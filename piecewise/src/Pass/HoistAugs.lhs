> {-# LANGUAGE RankNTypes, GADTs, FlexibleContexts, ScopedTypeVariables #-}
> {-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}

> module Pass.HoistAugs (hoisted, hoistedStmt, runHoisted, HoistError) where
> import Data.Foldable (traverse_)
> import Control.Eff
> import Control.Eff.Exception
> import Control.Eff.State.Lazy

> import IR.CST (Const(..), Var(..), varName)
> import IR.AST (Expr(..))
> import IR.AST (app)
> import qualified IR.AST.Initial as IA
> import IR.AST.Initial (Formals)
> import qualified IR.AST.Hoisted as HA
> import IR.AST.Hoisted (Stmt(..))
> import Util (Name(..), freshName, position)

> data HoistError = ReAssignment Var deriving Show

> type Hoisted a = forall r . (Member (Exc HoistError) r, Member (State Int) r)
>                           => Eff r a

> runHoisted :: Int -> Hoisted a -> Either HoistError (Int, a)
> runHoisted counter m = run (runExc (runState counter m))

The hoisted* functions mostly just fold over the tree. The only interesting case
is the Block case of `hoisted` which uses execState to reorder statements
and traverses the reordered statements.

> hoisted :: IA.Expr -> Hoisted HA.Expr
> hoisted (Fn pos cases) = Fn pos <$> traverse hoistCase cases
>     where hoistCase (formals, Just cond, expr) =
>               (formals,,) <$> (Just <$> hoisted cond) <*> hoisted expr
>           hoistCase (formals, Nothing, expr) =
>               (formals, Nothing,) <$> hoisted expr
> hoisted (Block pos stmts) =
>     do stmts' <- reverse <$> execState [] (traverse_ hoistStmt stmts)
>        Block pos <$> (traverse hoistedStmt stmts')
> hoisted (App pos f i args) =
>     App pos <$> hoisted f <*> hoisted i <*> hoisted args
> hoisted (PrimApp pos op args) = PrimApp pos op <$> traverse hoisted args
> hoisted (Var v) = return (Var v)
> hoisted (Const c) = return (Const c)

> hoistedStmt :: IA.Stmt -> Hoisted Stmt
> hoistedStmt (IA.Def var val) = Def var <$> hoisted val
> hoistedStmt (IA.Label label stmt) = Label label <$> hoistedStmt stmt
> hoistedStmt (IA.Guard cond jmp) = Guard <$> hoisted cond <*> pure jmp
> hoistedStmt (IA.Return expr) = Return <$> hoisted expr
> hoistedStmt (IA.Expr expr) = Expr <$> hoisted expr

When we are actually reordering statements we need to keep track of the new
statement sequence being built in addition to the possibility of error.

> type Hoisting a =
>     forall r . (Member (Exc HoistError) r,
>                 Member (State Int) r, Member (State [IA.Stmt]) r) => Eff r a

Since a list is being used as a statement list builder we need to reverse it at
the end in addition to running effects.

> push :: IA.Stmt -> Hoisting ()
> push stmt = modify (stmt :)

> assocDef :: Var -> IA.Expr -> Hoisting ()
> assocDef var val = do res::[IA.Stmt] <- get
>                       if any cond res
>                       then throwExc (ReAssignment var)
>                       else push (IA.Def var val)
>     where cond (IA.Def var' _) | varName var == varName var' = True
>           cond _ = False

> assocAugDef :: Var -> IA.Expr -> Hoisting ()
> assocAugDef var @ (position -> pos) val =
>     do tmp <- LexVar pos <$> freshName "t"
>        push (IA.Def tmp val)
>        modify (\res -> maybe (defaultDef tmp : res) id (assoc (Var tmp) res))
>     where assoc tmp ((IA.Def var' val'):stmts) | varName var == varName var' =
>               Just (mkDef var' val' tmp : stmts)
>           assoc tmp (stmt:stmts) = (stmt :) <$> assoc tmp stmts
>           assoc _ [] = Nothing
>           mkDef v f g = IA.Def v (mergeFns f g)
>           defaultDef tmp =
>               mkDef var (Var (UpperLexVar pos (varName var))) (Var tmp)
>           mergeFns f g = app pos (Var (LexVar pos (PlainName "fnMerge")))
>                                  (Const (Int pos 0)) [f, g]

> hoistStmt :: IA.Stmt -> Hoisting ()
> hoistStmt (IA.Def var val) = assocDef var val
> hoistStmt (IA.AugDef var val) = assocAugDef var val
> hoistStmt stmt @ (IA.Label _ _) = push stmt -- might misbehave when input does
> hoistStmt stmt @ (IA.Guard _ _) = push stmt
> hoistStmt stmt @ (IA.Return _) = push stmt
> hoistStmt stmt @ (IA.Expr _) = push stmt
