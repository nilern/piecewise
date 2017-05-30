{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module IR.AST (Stmt(..), stmtBinders, app, Expr(..), Formals(..)) where
import Data.Semigroup ((<>))
import Data.Foldable (foldl')
import qualified Text.PrettyPrint.Leijen.Text as P
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (</>))

import IR.CST (Var, Const)
import qualified Ops
import Ops (Primop)
import Util (Label, Pos, showViaPretty)

data Stmt = Def Var Expr
          | AugDef Var Expr
          | Guard Expr Label
          | Label Label Stmt
          | Expr Expr

data Expr = Fn Pos [(Formals, Maybe Expr, Expr)]
          | Block Pos [Stmt]
          | App Pos Expr Expr Expr
          | PrimApp Pos Primop [Expr]
          | Var Var
          | Const Const

data Formals = Formals { self :: Var, methodIndex :: Var, args :: Var }

stmtBinders :: Stmt -> [Var]
stmtBinders (Def v _) = [v]
stmtBinders (AugDef v _) = [v]
stmtBinders (Guard _ _) = []
stmtBinders (Label _ stmt) = stmtBinders stmt
stmtBinders (Expr _) = []

app :: Pos -> Expr -> Expr -> [Expr] -> Expr
app pos f i args = App pos f i (PrimApp pos Ops.Tuple args)

instance Pretty Stmt where
    pretty (Def pat val) = pretty pat <+> P.text "=" <+> pretty val
    pretty (AugDef pat val) = pretty pat <+> P.text "+=" <+> pretty val
    pretty (Guard cond dest) =
        "@guard" <+> pretty cond <+> P.text "=>" <+> pretty dest
    pretty (Label label stmt) = pretty label <> P.colon <+> pretty stmt
    pretty (Expr expr) = pretty expr

instance Show Stmt where
    show = showViaPretty

instance Pretty Expr where
    pretty (Fn _ cases) =
        P.braces (P.align (P.vcat (P.punctuate P.semi (prettyCase <$> cases))))
        where prettyCase (formals, Nothing, body) =
                  pretty formals <+> P.text "=>" <+> pretty body
              prettyCase (formals, Just cond, body) =
                  pretty formals <+> P.text "|" <+> pretty cond <+>
                      P.text "=>" <+> pretty body
    pretty (Block _ stmts) =
        P.lbrace <>
        P.nest 4 (P.line <> P.vcat (P.punctuate P.semi (pretty <$> stmts))) <>
        P.line <> P.rbrace
    pretty (App _ f i args) =
        P.parens $ P.align $ pretty f </> pretty i </> pretty args
    pretty (PrimApp _ Ops.Tuple args) =
        P.parens $ P.align $ P.fillSep $ P.punctuate P.comma (pretty <$> args)
    pretty (PrimApp _ op args) =
        P.parens $ P.align $ foldl' (</>) (pretty op) (pretty <$> args)
    pretty (Var v) = pretty v
    pretty (Const c) = pretty c

instance Show Expr where
    show = showViaPretty

instance Pretty Formals where
    pretty (Formals {self, methodIndex, args}) =
        P.text "self:" <+> pretty self <> P.comma <+>
            P.text "mi:" <+> pretty methodIndex <> P.comma <+>
            P.text "args:" <+> pretty args
