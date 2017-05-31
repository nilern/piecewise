{-# LANGUAGE OverloadedStrings #-}

module Ops (Primop(..), opMap) where
import Data.Text (Text)
import Data.HashMap.Lazy (HashMap, fromList)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

-- TODO: Enforce arities by fusing Primop with PrimApp

data Primop = IAdd | IEq | WordSize | Tuple | LoadWord | FnMerge | ThrowBindErr
            deriving Show

instance Pretty Primop where
    pretty = pretty . show

opMap :: HashMap Text Primop
opMap = fromList [("iadd", IAdd)
                 ,("ieq", IEq)
                 ,("wordSize", WordSize)
                 ,("tuple", Tuple)
                 ,("loadWord", LoadWord)
                 ,("fnMerge", FnMerge)
                 ,("ThrowBindErr", ThrowBindErr)
                 ]
