> module Interpreter.Cont
>        (Cont(..), ContDump, emptyDump, pushCont, popCont, splitDump) where
> import AST (Expr, Stmt)
> import Ops (Primop)
> import Interpreter.Env (LexEnv, DynEnv)
> import Util (Name)

Continuations
=============

> data Cont k v = Stmt (LexEnv k v) (DynEnv k v) [Stmt] (Cont k v)
>               | Applicant (LexEnv k v) (DynEnv k v) [Expr] (Cont k v)
>               | Arg (LexEnv k v) (DynEnv k v) v [v] [Expr] (Cont k v)
>               | PrimArg (LexEnv k v) (DynEnv k v) Primop [v] [Expr] (Cont k v)
>               | LexAssign (LexEnv k v) (DynEnv k v) Name (Cont k v)
>               | DynAssign (LexEnv k v) (DynEnv k v) Name (Cont k v)
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
