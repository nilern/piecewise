module Parsing.CST (Stmt(..), Expr(..), Var(..), Const(..)) where
import Data.Text (Text)
import Ops (Primop)
import Util (Pos, Positioned(..))

data Stmt = Def Expr Expr
          | AugDef Expr Expr
          | Expr Expr
          deriving Show

data Expr = Fn Pos [([Expr], Expr, Expr)]
          | Block Pos [Stmt]
          | App Pos Expr [Expr]
          | PrimApp Pos Primop [Expr]
          | Var Var
          | Const Const
          deriving Show

data Var = LexVar Pos Text
         | DynVar Pos Text
         deriving Show

data Const = Int Pos Int
           | Char Pos Text
           | String Pos Text
           | Keyword Pos Text
           deriving Show

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
    position (DynVar pos _) = pos

instance Positioned Const where
    position (Int pos _) = pos
    position (Char pos _) = pos
    position (String pos _) = pos
    position (Keyword pos _) = pos
