{-# LANGUAGE OverloadedStrings #-}

module AST (Stmt(..), stmtBinders, Expr(..), Jump(..)) where
import Data.Semigroup ((<>))
import Data.Foldable (foldl')
import qualified Text.PrettyPrint.Leijen.Text as P
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (</>))

import Parsing.CST (Var, Const)
import Ops (Primop)
import Util (showViaPretty, Pos)

data Stmt = Def Var Expr
          | AugDef Var Expr
          | Guard Expr Jump
          | Expr Expr

data Expr = Fn Pos [([Var], Expr)] -- TODO: Fn Pos [([Var], Maybe Expr, Expr)]
          | Block Pos [Stmt]
          | App Pos Expr [Expr]
          | PrimApp Pos Primop [Expr]
          | Var Var
          | Const Const

data Jump = NextMethod
          | ThrowBindErr
          deriving Show

stmtBinders :: Stmt -> [Var]
stmtBinders (Def v _) = [v]
stmtBinders (AugDef v _) = [v]
stmtBinders (Guard _ _) = []
stmtBinders (Expr _) = []

instance Pretty Stmt where
    pretty (Def pat val) = pretty pat <+> P.text "=" <+> pretty val
    pretty (AugDef pat val) = pretty pat <+> P.text "+=" <+> pretty val
    pretty (Guard cond dest) =
        "@guard" <+> pretty cond <+> P.text "=>" <+> pretty dest
    pretty (Expr expr) = pretty expr

instance Show Stmt where
    show = showViaPretty

instance Pretty Expr where
    pretty (Fn _ cases) =
        P.braces (P.align (P.vcat (P.punctuate P.semi (prettyCase <$> cases))))
        where prettyCase (formals, body) =
                  P.hsep (pretty <$> formals) <+> P.text "=>" <+> pretty body
    pretty (Block _ stmts) =
        P.lbrace <>
        P.nest 4 (P.line <> P.vcat (P.punctuate P.semi (pretty <$> stmts))) <>
        P.line <> P.rbrace
    pretty (App _ f args) =
        P.parens $ P.align $ foldl' (</>) (pretty f) (pretty <$> args)
    pretty (PrimApp _ op args) =
        P.parens $ P.align $ foldl' (</>) (pretty op) (pretty <$> args)
    pretty (Var v) = pretty v
    pretty (Const c) = pretty c

instance Show Expr where
    show = showViaPretty

instance Pretty Jump where
    pretty = pretty . show
