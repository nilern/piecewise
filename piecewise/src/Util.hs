{-# LANGUAGE DeriveGeneric #-}

module Util (Pos(..), nextPos, Positioned(..),
             Name(..), nameChars,
             ParseError(..), ItpError(..)) where
import qualified Data.Text as T
import Data.Default
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Pos = Pos !Int !Int !Int deriving Show

instance Default Pos where
    def = Pos 0 1 1

nextPos :: Pos -> Char -> Pos
nextPos (Pos a l c) '\t' = Pos (a + 1) l (((c + 8 - 1) `div` 8) * 8 + 1)
nextPos (Pos a l _) '\n' = Pos (a + 1) (l + 1) 1
nextPos (Pos a l c) _    = Pos (a + 1) l (c + 1)

class Positioned a where
    position :: a -> Pos

data Name = PlainName T.Text
          | UniqueName T.Text Int
          deriving (Show, Eq, Ord, Generic)

instance Hashable Name

nameChars :: Name -> T.Text
nameChars (PlainName cs) = cs
nameChars (UniqueName cs _) = cs

data ParseError t d e = MalformedNumber T.Text
                      | UnprecedentedOp T.Text
                      | UnexpectedInput T.Text
                      | UnmatchedDelims (Maybe d) d
                      | ParseError Pos t
                      | InvalidPattern Pos e
                      | WildDedent Pos Int Int
                      deriving Show

data ItpError b = BindingError b
                | StackUnderflow
                deriving Show
