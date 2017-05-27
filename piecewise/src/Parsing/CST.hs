{-# LANGUAGE OverloadedStrings #-}

module Parsing.CST (Stmt(..), Expr(..), Var(..), varName, Const(..)) where
import Data.Semigroup ((<>))
import Data.Foldable (foldl')
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.PrettyPrint.Leijen.Text as P
import Text.PrettyPrint.Leijen.Text (Pretty(..), (</>), (<+>))
import Ops (Primop)
import Util (showViaPretty, Name, nameChars, Pos, Positioned(..))

data Stmt = Def Expr Expr
          | AugDef Expr Expr
          | Expr Expr

instance Pretty Stmt where
    pretty (Def pat val) = pretty pat <+> P.text "=" <+> pretty val
    pretty (AugDef pat val) = pretty pat <+> P.text "+=" <+> pretty val
    pretty (Expr expr) = pretty expr

instance Show Stmt where
    show = showViaPretty

data Expr = Fn Pos [([Expr], Expr, Expr)]
          | Block Pos [Stmt]
          | App Pos Expr [Expr]
          | PrimApp Pos Primop [Expr]
          | Var Var
          | Const Const

instance Pretty Expr where
    pretty (Fn _ cases) =
        P.braces (P.align (P.vcat (P.punctuate P.semi (prettyCase <$> cases))))
         where prettyCase (pats, cond, body) =
                  P.hsep (pretty <$> pats) <+> P.text "|" <+>
                      pretty cond <+> P.text "=>" <+> pretty body
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

data Var = LexVar Pos Name  -- in lexical environment (register or closure)
         | UpperLexVar Pos Name
         | GlobVar Pos Name -- in global hashtable (REPL) or exe .text section
         | DynVar Pos Name  -- in dynamic environment intertwined with stack
         deriving Eq

instance Pretty Var where
    pretty (LexVar _ name) = pretty (nameChars name)
    pretty (UpperLexVar _ name) = pretty (nameChars name)
    pretty (GlobVar _ name) = pretty (nameChars name)
    pretty (DynVar _ name) = pretty ('$' `T.cons` nameChars name)

instance Show Var where
    show = showViaPretty

varName :: Var -> Name
varName (LexVar _ name) = name
varName (UpperLexVar _ name) = name
varName (GlobVar _ name) = name
varName (DynVar _ name) = name

data Const = Int Pos Int
           | Char Pos Text
           | String Pos Text
           | Keyword Pos Text

instance Pretty Const where
    pretty (Int _ i) = pretty i
    pretty (Char _ c) = P.squotes (pretty c)
    pretty (String _ s) = P.dquotes (pretty s)
    pretty (Keyword _ cs) = pretty (':' `T.cons` cs)

instance Show Const where
    show = showViaPretty

instance Positioned Stmt where
    position (Def pat _) = position pat
    position (AugDef pat _) = position pat
    position (Expr expr) = position expr

instance Positioned Expr where
    position (Fn pos _) = pos
    position (Block pos _) = pos
    position (App pos _ _) = pos
    position (PrimApp pos _ _) = pos
    position (Var v) = position v
    position (Const c) = position c

instance Positioned Var where
    position (LexVar pos _) = pos
    position (UpperLexVar pos _) = pos
    position (GlobVar pos _) = pos
    position (DynVar pos _) = pos

instance Positioned Const where
    position (Int pos _) = pos
    position (Char pos _) = pos
    position (String pos _) = pos
    position (Keyword pos _) = pos
