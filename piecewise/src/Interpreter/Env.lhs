> {-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving #-}

> module Interpreter.Env (Scoping, Env, LexEnv, DynEnv,
>                         empty, toplevel, pushFrame, lookup) where
> import Prelude hiding (lookup)
> import Data.Hashable (Hashable)
> import qualified Data.HashMap.Lazy as HM
> import Data.HashMap.Lazy (HashMap)
> import qualified Data.HashTable.IO as HT
> import Data.HashTable.IO (BasicHashTable)
> import Control.Applicative ((<|>))

> data Scoping = Lexical | Dynamic

> data Env :: Scoping -> * -> * -> * where
>     Simple :: (HashMap k v) -> Env s k v
>     Double :: (HashMap k v) -> (BasicHashTable k v) -> Env s k v

> deriving instance (Show k, Show v) => Show (Env s k v)

> type LexEnv k v = Env 'Lexical k v
> type DynEnv k v = Env 'Dynamic k v

> empty :: (Hashable k, Eq k) => Env s k v
> empty = Simple HM.empty

> toplevel :: (Hashable k, Eq k) => IO (Env s k v)
> toplevel = Double HM.empty <$> HT.new

> pushFrame :: (Hashable k, Eq k) => Env s k v -> [(k, v)] -> Env s k v
> pushFrame (Simple lbs) kvs = Simple (HM.union (HM.fromList kvs) lbs)
> pushFrame (Double lbs gbs) kvs = Double (HM.union (HM.fromList kvs) lbs) gbs

> lookup :: (Hashable k, Eq k) => Env s k v -> k -> v -> IO (Maybe v)
> lookup (Simple bindings) name _ = pure (HM.lookup name bindings)
> lookup (Double lbs gbs) name dflt =
>     do let lv = HM.lookup name lbs
>        gv <- HT.lookup gbs name
>        dv <- HT.insert gbs name dflt >> pure (Just dflt)
>        return (lv <|> gv <|> dv)
