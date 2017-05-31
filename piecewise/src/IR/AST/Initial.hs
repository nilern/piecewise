{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module IR.AST.Initial (Stmt(..), stmtBinders, Expr, addRet, Formals(..)) where
import Data.Semigroup ((<>))
import qualified Text.PrettyPrint.Leijen.Text as P
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>))

import IR.CST (Var)
import qualified IR.AST as AST
import Util (Label, showViaPretty)

data Stmt = Def Var Expr
          | AugDef Var Expr
          | Guard Expr Label
          | Label Label Stmt
          | Return Expr
          | Expr Expr

type Expr = AST.Expr Stmt Var Formals

data Formals = Formals { self :: Var, methodIndex :: Var, args :: Var }

stmtBinders :: Stmt -> [Var]
stmtBinders (Def v _) = [v]
stmtBinders (AugDef v _) = [v]
stmtBinders (Guard _ _) = []
stmtBinders (Label _ stmt) = stmtBinders stmt
stmtBinders (Return _) = []
stmtBinders (Expr _) = []

addRet :: [Stmt] -> [Stmt]
addRet [Expr expr] = [Return expr]
addRet (stmt:stmts) = stmt:addRet stmts

instance Pretty Stmt where
    pretty (Def pat val) = pretty pat <+> P.text "=" <+> pretty val
    pretty (AugDef pat val) = pretty pat <+> P.text "+=" <+> pretty val
    pretty (Guard cond dest) =
        "@guard" <+> pretty cond <+> P.text "=>" <+> pretty dest
    pretty (Return expr) = "@return" <+> pretty expr
    pretty (Label label stmt) = pretty label <> P.colon <+> pretty stmt
    pretty (Expr expr) = pretty expr

instance Show Stmt where
    show = showViaPretty

instance Pretty Formals where
    pretty (Formals {self, methodIndex, args}) =
        P.text "self:" <+> pretty self <> P.comma <+>
            P.text "mi:" <+> pretty methodIndex <> P.comma <+>
            P.text "args:" <+> pretty args
