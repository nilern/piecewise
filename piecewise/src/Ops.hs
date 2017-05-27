module Ops (Primop(..)) where
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data Primop = FnMerge deriving Show

instance Pretty Primop where
    pretty = pretty . show
