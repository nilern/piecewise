module AST (Expr(..), Const(..), Stmt(..), BlockItem(..), Pattern(..)) where
import qualified Data.Text as T
import Util (Pos, Positioned(..))

data Expr = Fn Pos [([Expr], [Stmt])]
          | Block Pos [Stmt]
          | Call Pos Expr [Expr]
          | Var Pos T.Text
          | Const Const
          deriving Show

data Const = Int Pos Int
           | String Pos T.Text
           | Char Pos T.Text
           deriving Show

data Stmt = Def Expr Expr
          | AugDef Expr Expr
          | Expr Expr
          deriving Show

data BlockItem = Clause [Expr] Stmt
               | Stmt Stmt
               deriving Show

data Pattern = PVar Pos T.Text
             | PInt Pos Int
             | PString Pos T.Text
             | PChar Pos T.Text
             | PTuple Pos [Pattern]
             | PArray Pos [Pattern]
             | PMap Pos [(Pattern, Pattern)]
             | PSet Pos [Pattern]
             deriving Show

instance Positioned Expr where
    position (Fn pos _) = pos
    position (Block pos _) = pos
    position (Call pos _ _) = pos
    position (Var pos _) = pos
    position (Const (Int pos _)) = pos
    position (Const (String pos _)) = pos
    position (Const (Char pos _)) = pos
