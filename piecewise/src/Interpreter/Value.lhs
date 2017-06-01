> {-# LANGUAGE FlexibleContexts #-}

> module Interpreter.Value (RedirectErr, Value(..), isTruthy,
>                           Method(..), method,
>                           emptyRedirect, unwrap, normalize) where
> import Data.IORef (IORef, newIORef, readIORef)
> import Data.Text (Text, unpack)
> import Data.List (intercalate)
> import Control.Eff
> import Control.Eff.Exception
> import Control.Eff.Lift

> import IR.CST (Var)
> import IR.AST.Initial (Formals)
> import IR.AST.Hoisted (Expr)
> import qualified Interpreter.Env as Env
> import Util (Name)

> data RedirectErr = UnAssigned Var deriving Show

> type LexEnv = Env.LexEnv Name Value

> data Value = Closure [Method]
>            | Tuple [Value]
>            | Int Int
>            | Bool Bool
>            | String Text
>            | Keyword Text
>            | Redirect Var (IORef (Maybe Value))

> isTruthy :: Value -> Bool
> isTruthy (Bool b) = b
> isTruthy _ = True

> newtype Method = Method (Formals, Maybe Expr, Expr, LexEnv)

> method :: LexEnv -> (Formals, Maybe Expr, Expr) -> Method
> method lEnv (formals, cond, body) = Method (formals, cond, body, lEnv)

> emptyRedirect :: SetMember Lift (Lift IO) r => Var -> Eff r Value
> emptyRedirect var = Redirect var <$> lift (newIORef Nothing)

> unwrap :: (Member (Exc RedirectErr) r, SetMember Lift (Lift IO) r)
>        => Value -> Eff r Value
> unwrap (Redirect var ref) = do ov <- lift $ readIORef ref
>                                case ov of
>                                    Just v -> unwrap v
>                                    Nothing -> throwExc (UnAssigned var)
> unwrap v = return v

> normalize :: (Member (Exc RedirectErr) r, SetMember Lift (Lift IO) r)
>           => Value -> Eff r Value
> normalize (Tuple vs) = Tuple <$> traverse normalize vs
> normalize v = unwrap v

> instance Show Value where
>     show (Closure methods) = "#<Fn (" ++ show (length methods) ++ " methods)>"
>     show (Tuple [v]) = '(' : show v ++ ",)"
>     show (Tuple vs) = '(' : intercalate ", " (map show vs) ++ ")"
>     show (Int i) = show i
>     show (Bool b) = show b
>     show (String t) = show t
>     show (Keyword cs) = ':' : unpack cs
>     show (Redirect _ _) = "#<Redirect>"
