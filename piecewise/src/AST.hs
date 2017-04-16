module AST (Exp(..), Stmt(..), BlockItem(..), Pattern(..), Positioned(..),
            exprPattern) where
import qualified Data.Text as T
import Lexer (Pos)

data Exp = Fn Pos [([Pattern], [Stmt])]
         | Block Pos [Stmt]
         | Call Pos Exp [Exp]
         | Var Pos T.Text
         | Int Pos Int
         | String Pos T.Text
         | Char Pos T.Text
         | Tuple Pos [Exp]
         | Array Pos [Exp]
         | Map Pos [(Exp, Exp)]
         | Set Pos [Exp]
         deriving Show

data Stmt = Def Pattern Exp
          | AugDef Pattern Exp
          | Expr Exp
          deriving Show

data BlockItem = Clause [Exp] Stmt
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

class Positioned a where
    position :: a -> Pos

instance Positioned Exp where
    position (Fn pos _) = pos
    position (Block pos _) = pos
    position (Call pos _ _) = pos
    position (Var pos _) = pos
    position (Int pos _) = pos
    position (String pos _) = pos
    position (Char pos _) = pos
    position (Tuple pos _) = pos
    position (Array pos _) = pos
    position (Map pos _) = pos
    position (Set pos _) = pos

exprPattern :: Exp -> Pattern
exprPattern (Var pos name) = PVar pos name
exprPattern (Int pos i) = PInt pos i
exprPattern (String pos s) = PString pos s
exprPattern (Char pos c) = PChar pos c
exprPattern (Tuple pos pats) = PTuple pos $ map exprPattern pats
exprPattern (Array pos pats) = PArray pos $ map exprPattern pats
exprPattern (Map pos pats) =
    PMap pos $ map (\(p, q) -> (exprPattern p, exprPattern q)) pats
exprPattern (Set pos pats) = PSet pos $ map exprPattern pats
