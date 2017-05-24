module AST (Stmt(..), Expr(..), Jump(..)) where
import Parsing.CST (Var, Const)
import Ops (Primop)
import Util (Pos, Name)

data Stmt = Def Var Expr
          | AugDef Var Expr
          | Guard Expr Jump
          | Expr Expr
          deriving Show

data Expr = Fn Pos [([Name], Expr)]
          | Block Pos [Stmt]
          | App Pos Expr [Expr]
          | PrimApp Pos Primop [Expr]
          | Var Var
          | Const Const
          deriving Show

data Jump = NextMethod
          | ThrowBindErr
          deriving Show
