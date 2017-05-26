module AST (Stmt(..), Expr(..), Jump(..)) where
import Data.List (intercalate)
import Parsing.CST (Var, Const)
import Ops (Primop)
import Util (Pos)

data Stmt = Def Var Expr
          | AugDef Var Expr
          | Guard Expr Jump
          | Expr Expr

instance Show Stmt where
    show (Def var val) = show var ++ " = " ++ show val
    show (AugDef var val) = show var ++ " += " ++ show val
    show (Guard cond dest) = "@guard " ++ show cond ++ " => " ++ show dest
    show (Expr expr) = show expr

data Expr = Fn Pos [([Var], Expr)]
          | Block Pos [Stmt]
          | App Pos Expr [Expr]
          | PrimApp Pos Primop [Expr]
          | Var Var
          | Const Const

instance Show Expr where
    show (Fn _ cases) = '{' : intercalate ";\n" (showCase <$> cases) ++ "}"
        where showCase (vars, body) =
                  intercalate " " (show <$> vars) ++ " => " ++ show body
    show (Block _ stmts) = '{' : intercalate ";\n" (show <$> stmts) ++ "}"
    show (App _ f args) = '(' : intercalate " " (show <$> f:args) ++ ")"
    show (PrimApp _ op args) =
        '(' : show op ++ ' ' : intercalate " " (show <$> args) ++ ")"
    show (Var var) = show var
    show (Const c) = show c

data Jump = NextMethod
          | ThrowBindErr
          deriving Show
