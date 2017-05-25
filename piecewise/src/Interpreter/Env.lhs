> {-# LANGUAGE FlexibleContexts #-}

> module Interpreter.Env (lookup, insert, BindingError(..), pushFrame,
>                         LexEnv, emptyLexEnv,
>                         DynEnv, emptyDynEnv) where
> import Prelude hiding (lookup)
> import Data.Typeable (Typeable)
> import qualified Data.HashTable.IO as H
> import Data.Hashable (Hashable)
> import Control.Eff
> import Control.Eff.Lift
> import Control.Eff.Exception
> import qualified Util

Environment Interface
=====================

> data BindingError k = Unbound k
>                     | ReAssignment k
>                     deriving Show

> type ItpError k = Util.ItpError (BindingError k)

> class Environment e where
>     bindings :: (Hashable k, Eq k) => e k v -> H.BasicHashTable k v
>     parent :: (Hashable k, Eq k) => e k v -> Maybe (e k v)
>     pushFrame :: (Hashable k, Eq k) => e k v -> IO (e k v)

> lookup :: (Environment e, Hashable k, Eq k, Typeable k,
>            Member (Exc (ItpError k)) r, SetMember Lift (Lift IO) r)
>        => e k v -> k -> Eff r v
> lookup env name =
>     do ov <- lift (H.lookup (bindings env) name)
>        case ov of
>            Just value -> return value
>            Nothing -> case parent env of
>                           Just p-> lookup p name
>                           Nothing ->
>                               throwExc (Util.BindingError (Unbound name))

> insert :: (Environment e, Hashable k, Eq k, Typeable k,
>            Member (Exc (ItpError k)) r, SetMember Lift (Lift IO) r)
>        => e k v -> k -> v -> Eff r ()
> insert env name value =
>     let kvs = bindings env in
>     do ov <- lift (H.lookup kvs name)
>        case ov of
>            Nothing -> lift (H.insert kvs name value)
>            Just _ -> throwExc (Util.BindingError (ReAssignment name))

Lexical Environment
-------------------

> data LexEnv k v = LexEnv (LexEnv k v) (H.BasicHashTable k v)
>                 | GlobalEnv (H.BasicHashTable k v)

> emptyLexEnv :: IO (LexEnv k v)
> emptyLexEnv = GlobalEnv <$> H.new

> instance Environment LexEnv where
>     bindings (LexEnv _ kvs) = kvs
>     bindings (GlobalEnv kvs) = kvs
>     parent (LexEnv p _) = Just p
>     parent (GlobalEnv _) = Nothing
>     pushFrame p = LexEnv p <$> H.new

Dynamic Environment
-------------------

> data DynEnv k v = DynEnv (Maybe (DynEnv k v)) (H.BasicHashTable k v)

> emptyDynEnv :: IO (DynEnv k v)
> emptyDynEnv = DynEnv Nothing <$> H.new

> instance Environment DynEnv where
>     bindings (DynEnv _ kvs) = kvs
>     parent (DynEnv p _) = p
>     pushFrame p = DynEnv (Just p) <$> H.new
