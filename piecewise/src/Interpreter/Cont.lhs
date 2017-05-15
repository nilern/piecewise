> module Interpreter.Cont
>        (Cont(..), ContDump, emptyDump, pushCont, popCont, splitDump) where
> import Data.Text (Text)
> import Interpreter.Env (LexEnv, DynEnv)

Continuations
=============

> data Cont k v = LexAssign (Cont k v) (LexEnv k v) (DynEnv k v) Text
>               | DynAssign (Cont k v) (LexEnv k v) (DynEnv k v) Text
>               | Halt

Dumps
=====

> type Prompt = Int
> data ContDump k v = CDump [(Prompt, Cont k v)]

> emptyDump :: ContDump k v
> emptyDump = CDump []

> pushCont :: ContDump k v -> Prompt -> Cont k v -> ContDump k v
> pushCont (CDump pks) p k = CDump ((p, k) : pks)

> popCont :: ContDump k v -> Maybe (Cont k v, ContDump k v)
> popCont (CDump ((_, k):pks)) = Just (k, CDump pks)
> popCont (CDump []) = Nothing

> splitDump :: ContDump k v -> Prompt -> Maybe (ContDump k v, ContDump k v)
> splitDump (CDump pks) p =
>     case break ((== p) . fst) pks of
>         (_, []) -> Nothing
>         (pks', pks'') -> Just (CDump pks', CDump pks'')
