module AST (Exp(..), Stmt(..), BlockItem(..), Pattern(..), exprPattern) where
import qualified Data.Text as T

data Exp = Fn [([Pattern], [Stmt])]
         | Block [Stmt]
         | Call Exp [Exp]
         | Var T.Text
         | Int Int
         | String T.Text
         | Char T.Text
         | Tuple [Exp]
         | Array [Exp]
         | Map [(Exp, Exp)]
         | Set [Exp]
         deriving Show

data Stmt = Def Pattern Exp
          | AugDef Pattern Exp
          | Expr Exp
          deriving Show

data BlockItem = Clause [Exp] Stmt
               | Stmt Stmt
               deriving Show

data Pattern = PVar T.Text
             | PInt Int
             | PString T.Text
             | PChar T.Text
             | PTuple [Pattern]
             | PArray [Pattern]
             | PMap [(Pattern, Pattern)]
             | PSet [Pattern]
             deriving Show

exprPattern :: Exp -> Pattern
exprPattern (Var name) = PVar name
exprPattern (Int i) = PInt i
exprPattern (String s) = PString s
exprPattern (Char c) = PChar c
exprPattern (Tuple pats) = PTuple $ map exprPattern pats
exprPattern (Array pats) = PArray $ map exprPattern pats
exprPattern (Map pats) =
    PMap $ map (\(p, q) -> (exprPattern p, exprPattern q)) pats
exprPattern (Set pats) = PSet $ map exprPattern pats
