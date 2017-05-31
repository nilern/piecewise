module Ops (Primop(..)) where
import Text.PrettyPrint.Leijen.Text (Pretty(..))

-- TODO: Enforce arities by fusing Primop with PrimApp

data Primop = IAdd | IEq | WordSize | Tuple | LoadWord | FnMerge | ThrowBindErr
            deriving Show

instance Pretty Primop where
    pretty = pretty . show
