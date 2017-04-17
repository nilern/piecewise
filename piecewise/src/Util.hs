module Util (Pos(..), Positioned(..), ParseError(..)) where
import qualified Data.Text as T
import Data.Default

data Pos = Pos !Int !Int !Int deriving Show

instance Default Pos where
    def = Pos 0 1 1

class Positioned a where
    position :: a -> Pos

data ParseError t d e = MalformedNumber T.Text
                      | UnprecedentedOp T.Text
                      | UnexpectedInput T.Text
                      | UnmatchedDelims (Maybe d) d
                      | ParseError Pos t
                      | InvalidPattern Pos e
                      deriving Show
