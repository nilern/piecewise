module AST (Exp(Fn, Block, Int, Set),
            Stmt(Def, AugDef, Expr),
            BlockItem(Clause, Stmt)) where

data Exp = Fn [([Exp], [Stmt])]
         | Block [Stmt]
         | Int Int
         | Set [Exp]
         deriving Show

data Stmt = Def Exp Exp
          | AugDef Exp Exp
          | Expr Exp
          deriving Show

data BlockItem = Clause [Exp] Stmt
               | Stmt Stmt
               deriving Show
