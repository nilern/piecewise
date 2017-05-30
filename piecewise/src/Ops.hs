module Ops (Primop(..)) where
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data Primop = IAdd | Tuple | FnMerge | ThrowBindErr deriving Show

instance Pretty Primop where
    pretty = pretty . show
