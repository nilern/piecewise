module AST (Stmt(..), Expr(..), Var(..), Const(..)) where
import qualified Data.Text as T
import Util (Pos, Positioned(..))

data Stmt = Def Expr Expr
          | AugDef Expr Expr
          | Expr Expr
          deriving Show

data Expr = Fn Pos [([Expr], [Stmt])]
          | Block Pos [Stmt]
          | Call Pos Expr [Expr]
          | Var Var
          | Const Const
          deriving Show

data Var = LexVar Pos T.Text
         | DynVar Pos T.Text
         deriving Show

data Const = Int Pos Int
           | String Pos T.Text
           | Char Pos T.Text
           deriving Show

instance Positioned Stmt where
    position (Def pat _) = position pat
    position (AugDef pat _) = position pat
    position (Expr expr) = position expr

instance Positioned Expr where
    position (Fn pos _) = pos
    position (Block pos _) = pos
    position (Call pos _ _) = pos
    position (Var v) = position v
    position (Const c) = position c

instance Positioned Var where
    position (LexVar pos _) = pos
    position (DynVar pos _) = pos

instance Positioned Const where
    position (Int pos _) = pos
    position (String pos _) = pos
    position (Char pos _) = pos
