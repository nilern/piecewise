> {-# LANGUAGE TupleSections #-}

> module HoistAugs (hoistAugs, hoistAugStmts, ReAssignment) where
> import Data.Function ((&))
> import Control.Monad.State
> import Control.Monad.Except
> import Control.Monad.Identity
> import AST (Expr(..), Stmt(..))

> data ReAssignment = ReAssignment Stmt deriving Show

> hoistAugs :: Expr -> Either ReAssignment Expr
> hoistAugs (Fn pos cases) = Fn pos <$> (traverse hoistCase cases)
>     where hoistCase (formals, stmts) = (formals,) <$> hoistAugStmts stmts
> hoistAugs (Block pos stmts) = Block pos <$> hoistAugStmts stmts
> hoistAugs (Call pos f args) =
>     Call pos <$> hoistAugs f <*> traverse hoistAugs args
> hoistAugs node @ (Var _) = return node
> hoistAugs node @ (Const _) = return node

> hoistAugStmts :: [Stmt] -> Either ReAssignment [Stmt]
> hoistAugStmts stmts = runHoisting hoisting stmts
>     where hoisting = do continue <- step return
>                         if continue then hoisting else return ()
>           processStmt def @ (Def pat _) =
>               do res <- gets result
>                  case lookupDef res pat of
>                      Just _ -> throwError (ReAssignment def)
>                      Nothing -> return def
>           processStmt (AugDef pat val) =
>               do res <- gets result
>                  case lookupDef res pat of
>                      Just (Def pat' val') ->
>                      Nothing ->
>           processStmt expr @ (Expr _) = return expr

> data HoistingState = HState {todo :: [Stmt], result :: BlockBuilder}

> newtype BlockBuilder = BlockBuilder [Stmt]

> startState :: [Stmt] -> HoistingState
> startState stmts = HState stmts (BlockBuilder [])

Find the definition of `name` in `rstmts`. We can just take the first one.

> lookupDef :: BlockBuilder -> Text -> Maybe Stmt
> lookupDef (BlockBuilder ((def @ (Def dname _))::_)) name | dname == name =
>     Just def
> lookupDef (BlockBuilder (_::stmts))
> lookupDef (BlockBuilder []) _ = Nothing

Add a def for `name` to `rstmts`. If one already exists, signal an error:

> assocDef :: BlockBuilder -> Text -> Expr -> Either ReAssignment [Stmt]
> assocDef rstmts name val = Def name val : rstmts

Merge `method` into the definition of `name` in `rstmts`. If a definition is not
found, it is created using `name` from a parent scope:

> augmentDef :: [Stmt] -> Text -> Expr -> [Stmt]
> augmentDef rstmts name method =

> type Hoisting a = StateT HoistingState (ExceptT ReAssignment Identity) a

> runHoisting :: Hoisting a -> [Stmt] -> Either ReAssignment [Stmt]
> runHoisting hoisting stmts =
>    result <$> (execStateT hoisting (startState stmts)
>                & runExceptT
>                & runIdentity)

> step :: (Stmt -> Hoisting Stmt) -> Hoisting Bool
> step f = do HState stmts hstmts <- get
>             case stmts of
>                 stmt:stmts' ->
>                     do hstmt <- f stmt
>                        put (HState stmts' (hstmts ++ [hstmt]))
>                        return True
>                 [] -> return False
