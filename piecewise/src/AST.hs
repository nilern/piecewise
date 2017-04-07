module AST (Exp(Int, Set)) where

data Exp = Int Int
         | Set [Exp]
         deriving Show
