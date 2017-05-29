> {-# LANGUAGE FlexibleContexts, RankNTypes, GADTs #-}
> {-# LANGUAGE ViewPatterns, OverloadedStrings, NamedFieldPuns #-}

> module Pass.PatExpand (expandExpr, expandStmt, expandStmtList,
>                        runExpansion, PatError) where
> import Data.Foldable (toList)
> import Control.Monad (foldM)
> import Control.Eff
> import Control.Eff.Exception
> import Control.Eff.State.Lazy
> import Control.Eff.Reader.Lazy

> import qualified IR.CST as CST
> import IR.CST (Const(..), Var(..))
> import IR.AST (Stmt(..), Expr(..), Formals(..), Jump(..), app)
> import Util (Name(..), freshName, position)

> data PatError = InvalidPat CST.Expr deriving Show

> type Expansion a =
>     forall r . (Member (Exc PatError) r,
>                 Member (Reader Jump) r, Member (State Int) r)
>              => Eff r a

> runExpansion :: Expansion a -> Jump -> Int -> Either PatError (Int, a)
> runExpansion m jmp counter = run (runExc (runReader (runState counter m) jmp))

FIXME: Pack source level formal pats to tuple and generate unpacking code for it
here.

> expandExpr :: CST.Expr -> Expansion Expr
> expandExpr (CST.Fn pos cases) = Fn pos <$> traverse expandCase cases
>     where expandCase (pats, cond, body) =
>               local (const NextMethod)
>                   (do self <- LexVar pos <$> freshName "self"
>                       args <- LexVar pos <$> freshName "args"
>                       jmp <- ask
>                       let argc = length pats
>                       let argis = [0..argc - 1]
>                       let fgets = getArg (CST.Var args) <$> argis
>                       patStmts <- expandPatList Def pats fgets
>                       cond' <- expandCond cond
>                       body' <- Expr <$> expandExpr body
>                       return (Formals {self, args}, cond',
>                               Block pos (Guard (hasLen (Const (Int pos argc))
>                                                        (Var args)) jmp :
>                                          patStmts ++
>                                          (Expr <$> toList cond') ++
>                                          [body'])))
>           expandCond (Just cond) = Just <$> expandExpr cond
>           expandCond Nothing = pure Nothing
>           hasLen l e = app pos eq [l, (app pos count [e])]
>           eq = Var (LexVar pos (PlainName "=="))
>           count = Var (LexVar pos (PlainName "count"))
>           getArg args i = CST.App pos ref [args, CST.Const (Int pos i)]
>           ref = CST.Var (LexVar pos (PlainName "get"))
> expandExpr (CST.Block pos stmts) =
>     local (const ThrowBindErr) (Block pos <$> expandStmtList stmts)
> expandExpr (CST.App pos f args) =
>     app pos <$> expandExpr f <*> traverse expandExpr args
> expandExpr (CST.PrimApp pos op args) =
>     PrimApp pos op <$> traverse expandExpr args
> expandExpr (CST.Var v) = return (Var v)
> expandExpr (CST.Const c) = return (Const c)

> expandStmt :: CST.Stmt -> Expansion [Stmt]
> expandStmt (CST.Def pat val) = expandPat Def pat val
> expandStmt (CST.AugDef pat val) = expandPat AugDef pat val
> expandStmt (CST.Expr e) = (:[]) . Expr <$> expandExpr e

> expandStmtList :: [CST.Stmt] -> Expansion [Stmt]
> expandStmtList stmts = foldM (\stmts' stmt -> (stmts' ++) <$> expandStmt stmt)
>                              mempty stmts

TODO: CST.PrimApp

> expandPat :: (Var -> Expr -> Stmt) -> CST.Expr -> CST.Expr -> Expansion [Stmt]
> expandPat _ pat @ (CST.Fn _ _) _ = throwExc $ InvalidPat pat
> expandPat _ pat @ (CST.Block _ _) _ = throwExc $ InvalidPat pat
> expandPat mkDef (CST.App pos f args) val =
>     do f' <- expandExpr f
>        val' <- expandExpr val
>        oview <- LexVar pos <$> freshName "ovw"
>        view <- LexVar pos <$> freshName "vw"
>        jmp <- ask
>        (viewStmts oview view f' val' jmp ++) <$>
>            expandPatList mkDef args (map (field (CST.Var view)) [0..])
>     where viewStmts oview view f' val' jmp =
>               [Def oview (app pos (Var unapply) [f', val']),
>                Guard (isSome (Var oview)) jmp,
>                Def view (app pos (Var unwrap) [Var oview]),
>                Guard (hasLen (Const (Int pos argc)) (Var view)) jmp]
>           field v i = CST.App pos ref [v, CST.Const (Int pos i)]
>           unapply = LexVar pos (PlainName "unapply")
>           unwrap = LexVar pos (PlainName "unwrap")
>           isSome v = app pos (Var (LexVar pos (PlainName "some?"))) [v]
>           hasLen l e = app pos eq [l, (app pos count [e])]
>           eq = Var (LexVar pos (PlainName "=="))
>           count = Var (LexVar pos (PlainName "count"))
>           ref = CST.Var (LexVar pos (PlainName "get"))
>           argc = length args
> expandPat mkDef (CST.Var var) val = (:[]) . mkDef var <$> expandExpr val
> expandPat _ (CST.Const (c @ (position -> pos))) val =
>     do jmp <- ask
>        val' <- expandExpr val
>        return [Guard (app pos eq [val', Const c]) jmp]
>     where eq = Var (LexVar pos (PlainName "=="))

> expandPatList :: (Var -> Expr -> Stmt) -> [CST.Expr] -> [CST.Expr]
>               -> Expansion [Stmt]
> expandPatList mkDef pats vals =
>     foldM (\stmts (pat, val) -> (stmts ++) <$> expandPat mkDef pat val)
>           mempty (zip pats vals)
