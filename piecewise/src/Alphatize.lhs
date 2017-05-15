> module Alphatize (PatternError(..), alphatize, alphatizeStmt) where
> import qualified Data.Text as T
> import Control.Monad.State
> import Control.Monad.Except

> import AST (Expr(..), Stmt, Var(..))

> data PatternError = PatternError Expr deriving Show

> type Alphatization a = StateT (Int, [(T.Text, Name)]) (Either PatternError) a

> data Name = UniqueName T.Text Int

> rename :: T.Text -> Alphatization Name
> rename name = do (i, kvs) <- get
>                  put (i + 1, kvs)
>                  return $ UniqueName name i

> bindings :: Expr -> Alphatization ()
> bindings (Var (LexVar _ name)) =
>     do uname <- rename name
>        modify (\(i, kvs) -> (i, (name, uname):kvs))
> bindings (Const _) = return ()
> bindings e = throwError $ PatternError e

> alphatize :: Expr -> Alphatization Expr
> alphatize expr = return expr

> alphatizeStmt :: Stmt -> Alphatization Stmt
> alphatizeStmt stmt = return stmt
