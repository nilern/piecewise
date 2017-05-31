{-# LANGUAGE FlexibleContexts, DeriveGeneric #-}

module Util (showViaPretty,
             Pos(..), nextPos, Positioned(..),
             Name(..), nameChars, freshName,
             Label, freshLabel,
             ParseError(..)) where
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.PrettyPrint.Leijen.Text as P
import Text.PrettyPrint.Leijen.Text (Pretty(..), renderPretty)
import Data.Default
import Control.Eff
import Control.Eff.State.Lazy
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

showViaPretty :: (Pretty a) => a -> String
showViaPretty = show . renderPretty 1.0 80 . pretty

data Pos = Pos !Int !Int !Int deriving (Show, Eq)

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
          deriving (Eq, Ord, Generic)

instance Hashable Name

instance Pretty Name where
    pretty (PlainName cs) = pretty cs
    pretty (UniqueName cs i) = pretty cs <> P.int i

instance Show Name where
    show (PlainName cs) = T.unpack cs
    show (UniqueName cs i) = T.unpack cs ++ show i

nameChars :: Name -> T.Text
nameChars (PlainName cs) = cs
nameChars (UniqueName cs _) = cs

freshName :: (Member (State Int) r) => Text -> Eff r Name
freshName chars = do res <- UniqueName chars <$> get
                     modify (+ (1::Int))
                     return res

newtype Label = Label Int deriving (Show, Eq, Generic)

instance Hashable Label

instance Pretty Label where
    pretty (Label i) = P.int i

freshLabel :: (Member (State Int) r) => Eff r Label
freshLabel = do i <- get
                modify (+ (1::Int))
                return (Label i)

data ParseError t d e = MalformedNumber T.Text
                      | UnprecedentedOp T.Text
                      | UnexpectedInput T.Text
                      | UnmatchedDelims (Maybe d) d
                      | ParseError Pos t
                      | InvalidPattern Pos e
                      | WildDedent Pos Int Int
                      deriving Show
