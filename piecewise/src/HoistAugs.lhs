> {-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
> {-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}

> module HoistAugs (hoisted, hoistedStmt, HoistError) where
> import Data.Foldable (traverse_)
> import Control.Eff
> import Control.Eff.Exception
> import Control.Eff.State.Lazy
> import Parsing.CST (Var(..), varName)
> import AST (Expr(..), Stmt(..))
> import Util (Name(..), position)

> data HoistError = ReAssignment Var deriving Show

The hoisted* functions mostly just fold over the tree. The only interesting case
is the Block case of `hoisted` which uses execHoisting to reorder statements
and traverses the reordered statements.

> hoisted :: Expr -> Either HoistError Expr
> hoisted (Fn pos cases) = Fn pos <$> (traverse hoistCase cases)
>     where hoistCase (formals, expr) = (formals,) <$> hoisted expr
> hoisted (Block pos stmts) =
>     do stmts' <- execHoisting (traverse_ hoistStmt stmts)
>        Block pos <$> (traverse hoistedStmt stmts')
> hoisted (App pos f args) = App pos <$> hoisted f <*> traverse hoisted args
> hoisted (PrimApp pos op args) = PrimApp pos op <$> traverse hoisted args
> hoisted node @ (Var _) = return node
> hoisted node @ (Const _) = return node

> hoistedStmt :: Stmt -> Either HoistError Stmt
> hoistedStmt (Def var val) = Def var <$> hoisted val
> hoistedStmt (AugDef var val) = AugDef var <$> hoisted val
> hoistedStmt (Guard cond jmp) = Guard <$> hoisted cond <*> pure jmp
> hoistedStmt (Expr expr) = Expr <$> hoisted expr

When we are actually reordering statements we need to keep track of the new
statement sequence being built in addition to the possibility of error.

> type Hoisting a =
>     forall r . (Member (Exc HoistError) r, Member (State [Stmt]) r) => Eff r a

Since a list is being used as a statement list builder we need to reverse it at
the end in addition to running effects.

> execHoisting :: Hoisting () -> Either HoistError [Stmt]
> execHoisting h = reverse <$> run (runExc (execState [] h))

> push :: Stmt -> Hoisting ()
> push stmt = modify (stmt :)

> assocDef :: Var -> Expr -> Hoisting ()
> assocDef var val = do res::[Stmt] <- get
>                       if any cond res
>                       then throwExc (ReAssignment var)
>                       else push (Def var val)
>     where cond (Def var' _) | varName var == varName var' = True
>           cond _ = False

FIXME: the function-defining Expr gets lifted over guards in cases like

    f 0 = 1
    (f, g) += genFns ()

obviously this should neverever happen!

> assocAugDef :: Var -> Expr -> Hoisting ()
> assocAugDef var @ (position -> pos) val =
>     modify (\res -> maybe (defaultDef : res) id (assoc res))
>     where assoc ((Def var' val'):stmts) | varName var == varName var' =
>               Just (mkDef var' val' val : stmts)
>           assoc (stmt:stmts) = (stmt :) <$> assoc stmts
>           assoc [] = Nothing
>           mkDef v f g = Def v (mergeFns f g)
>           defaultDef = mkDef var (Var (UpperLexVar pos (varName var))) val
>           mergeFns f g = App pos (Var (LexVar pos (PlainName "fnMerge")))
>                              [f, g]

> hoistStmt :: Stmt -> Hoisting ()
> hoistStmt (Def var val) = assocDef var val
> hoistStmt (AugDef var val) = assocAugDef var val
> hoistStmt stmt @ (Guard _ _) = push stmt
> hoistStmt stmt @ (Expr _) = push stmt
