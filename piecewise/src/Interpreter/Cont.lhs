> module Interpreter.Cont
>        (Cont(..), frames,
>         ContDump, emptyDump, pushCont, popCont, splitDump) where
> import IR.CST (Var)
> import IR.AST (Expr, Stmt)
> import Ops (Primop)
> import Interpreter.Env (LexEnv, DynEnv)

Continuations
=============

> data Cont k v = Stmt [Stmt] (LexEnv k v) (DynEnv k v) (Cont k v)
>               | Applicant [Expr] (LexEnv k v) (DynEnv k v) (Cont k v)
>               | Arg v [v] [Expr] (LexEnv k v) (DynEnv k v) (Cont k v)
>               | PrimArg Primop [v] [Expr] (LexEnv k v) (DynEnv k v) (Cont k v)
>               | Assign Var (LexEnv k v) (DynEnv k v) (Cont k v)
>               | Halt

> frames :: Cont k v -> Maybe (LexEnv k v, DynEnv k v, Cont k v)
> frames (Stmt _ l d k) = Just (l, d, k)
> frames (Applicant _ l d k) = Just (l, d, k)
> frames (Arg _ _ _ l d k) = Just (l, d, k)
> frames (PrimArg _ _ _ l d k) = Just (l, d, k)
> frames (Assign _ l d k) = Just (l, d, k)
> frames Halt = Nothing

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
