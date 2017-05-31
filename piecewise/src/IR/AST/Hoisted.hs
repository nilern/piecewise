{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module IR.AST.Hoisted (Stmt(..), stmtBinders, Expr, addRet) where
import Data.Semigroup ((<>))
import qualified Text.PrettyPrint.Leijen.Text as P
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>))

import IR.CST (Var)
import qualified IR.AST as AST
import qualified IR.AST.Initial as IA
import Util (Label, showViaPretty)

data Stmt = Def Var Expr
          | Guard Expr Label
          | Label Label Stmt
          | Return Expr
          | Expr Expr

type Expr = AST.Expr Stmt Var IA.Formals

stmtBinders :: Stmt -> [Var]
stmtBinders (Def v _) = [v]
stmtBinders (Guard _ _) = []
stmtBinders (Label _ stmt) = stmtBinders stmt
stmtBinders (Return _) = []
stmtBinders (Expr _) = []

addRet :: [Stmt] -> [Stmt]
addRet [Expr expr] = [Return expr]
addRet (stmt:stmts) = stmt:addRet stmts

instance Pretty Stmt where
    pretty (Def pat val) = pretty pat <+> P.text "=" <+> pretty val
    pretty (Guard cond dest) =
        "@guard" <+> pretty cond <+> P.text "=>" <+> pretty dest
    pretty (Return expr) = "@return" <+> pretty expr
    pretty (Label label stmt) = pretty label <> P.colon <+> pretty stmt
    pretty (Expr expr) = pretty expr

instance Show Stmt where
    show = showViaPretty
