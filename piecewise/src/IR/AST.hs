{-# LANGUAGE OverloadedStrings #-}

module IR.AST (Expr(..), app) where
import Data.Semigroup ((<>))
import Data.Foldable (foldl')
import qualified Text.PrettyPrint.Leijen.Text as P
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (</>))

import IR.CST (Const)
import qualified Ops
import Ops (Primop)
import Util (Pos, showViaPretty)

-- TODO: enforce that the last Stmt in a Block is a Label or Expr

data Expr s v f = Fn Pos [(f, Maybe (Expr s v f), Expr s v f)]
                | Block Pos [s]
                | App Pos (Expr s v f) (Expr s v f) (Expr s v f)
                | PrimApp Pos Primop [Expr s v f]
                | Var v
                | Const Const

app :: Pos -> Expr s v f -> Expr s v f -> [Expr s v f] -> Expr s v f
app pos f i args = App pos f i (PrimApp pos Ops.Tuple args)

instance (Pretty s, Pretty v, Pretty f) => Pretty (Expr s v f) where
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

instance (Pretty s, Pretty v, Pretty f) => Show (Expr s v f) where
    show = showViaPretty
