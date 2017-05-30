> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE RankNTypes, GADTs #-}
> {-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TupleSections,
>              NamedFieldPuns, RecordWildCards #-}

> module Interpreter (Value, ItpError,
>                     eval, evalStmt, unwrap, normalize, evalInterpreter) where
> import Prelude hiding (lookup)
> import Data.IORef (IORef, newIORef, readIORef, writeIORef)
> import Data.List (intercalate)
> import Data.Text (Text)
> import Control.Lens (makeLenses, view, over, set)
> import Control.Eff
> import Control.Eff.Lift
> import Control.Eff.State.Lazy
> import Control.Eff.Exception

> import qualified IR.CST as CST (Const(..))
> import IR.CST (Var(..), varName, isLexVar, isDynVar)
> import qualified IR.AST as AST (Expr(..))
> import IR.AST (Expr, Stmt(..), stmtBinders, Formals(..))
> import Ops (Primop)
> import qualified Interpreter.Env as Env
> import Interpreter.Env (pushFrame)
> import qualified Ops
> import Util (Name, Pos)

Value Representation
====================

> data Value = Closure [Method]
>            | Tuple [Value]
>            | Int Int
>            | String Text
>            | Redirect Var (IORef (Maybe Value))

> newtype Method = Method (Formals, Maybe Expr, Expr, LexEnv)

> method :: LexEnv -> (Formals, Maybe Expr, Expr) -> Method
> method lEnv (formals, cond, body) = Method (formals, cond, body, lEnv)

> emptyRedirect :: Var -> IO Value
> emptyRedirect var = Redirect var <$> newIORef Nothing

> unwrap :: Value -> Interpreter Value
> unwrap (Redirect var ref) = do ov <- lift $ readIORef ref
>                                case ov of
>                                    Just v -> unwrap v
>                                    Nothing -> throwExc (UnAssigned var)
> unwrap v = return v

> normalize :: Value -> Interpreter Value
> normalize f @ (Closure _) = pure f
> normalize (Tuple vs) = Tuple <$> traverse normalize vs
> normalize i @ (Int _) = pure i
> normalize s @ (String _) = pure s
> normalize r @ (Redirect _ _) = normalize =<< unwrap r

> instance Show Value where
>     show (Closure methods) = "#<Fn (" ++ show (length methods) ++ " methods)>"
>     show (Tuple [v]) = '(' : show v ++ ",)"
>     show (Tuple vs) = '(' : intercalate ", " (map show vs) ++ ")"
>     show (Int i) = show i
>     show (String t) = show t
>     show (Redirect _ _) = "#<Redirect>"

Errors
======

> data ItpError = Unbound Pos Name
>               | UnAssigned Var
>               | ReAssignment Pos Name
>               | UnAssignable Pos Name
>               | StackUnderflow
>               deriving Show

Environments
============

> type LexEnv = Env.LexEnv Name Value
> type DynEnv = Env.DynEnv Name Value

Continuations
=============

> data Cont = Stmts [Stmt] Context
>           | Applicant Expr Expr Context
>           | MethodIndex Value Expr Context
>           | Arg Value Value Context
>           | PrimArg Primop [Value] [Expr] Context
>           | Assign Var Context
>           | Halt

> contCtx :: Cont -> Maybe Context
> contCtx (Stmts _ ctx) = Just ctx
> contCtx (Applicant _ _ ctx) = Just ctx
> contCtx (MethodIndex _ _ ctx) = Just ctx
> contCtx (Arg _ _ ctx) = Just ctx
> contCtx (PrimArg _ _ _ ctx) = Just ctx
> contCtx (Assign _ ctx) = Just ctx
> contCtx Halt = Nothing

Dumps
=====

> type Prompt = Int
> data ContDump = CDump [(Prompt, Cont)]

> emptyDump :: ContDump
> emptyDump = CDump []

> pushCont :: ContDump -> Prompt -> Cont -> ContDump
> pushCont (CDump pks) p k = CDump ((p, k) : pks)

> popCont :: ContDump -> Maybe (Cont, ContDump)
> popCont (CDump ((_, k):pks)) = Just (k, CDump pks)
> popCont (CDump []) = Nothing

> splitDump :: ContDump -> Prompt -> Maybe (ContDump, ContDump)
> splitDump (CDump pks) p =
>     case break ((== p) . fst) pks of
>         (_, []) -> Nothing
>         (pks', pks'') -> Just (CDump pks', CDump pks'')

Interpreter Monad
=================

> type Interpreter a =
>     forall r . (Member (Exc ItpError) r,
>                 Member (State Context) r,
>                 Member (State ContDump) r,
>                 SetMember Lift (Lift IO) r)
>              => Eff r a

> data Context = Context { _ctxLex :: LexEnv
>                        , _ctxDyn :: DynEnv
>                        , _ctxCont :: Cont }
> makeLenses ''Context

> evalInterpreter :: LexEnv -> DynEnv -> Interpreter a
>                 -> IO (Either ItpError a)
> evalInterpreter lEnv dEnv m =
>     runLift (runExc (evalState emptyDump (evalState ctx m)))
>     where ctx::Context = Context lEnv dEnv Halt

> getLexEnv :: (Member (State Context) r) => Eff r LexEnv
> getLexEnv = view ctxLex <$> get

> putLexEnv :: (Member (State Context) r) => LexEnv -> Eff r ()
> putLexEnv lEnv = modify (set ctxLex lEnv)

> getDynEnv :: (Member (State Context) r) => Eff r DynEnv
> getDynEnv = view ctxDyn <$> get

> putDynEnv :: (Member (State Context) r) => DynEnv -> Eff r ()
> putDynEnv dEnv = modify (set ctxDyn dEnv)

> lookup :: Var -> Interpreter Value
> lookup var =
>     do dflt <- lift $ emptyRedirect var
>        case var of
>            LexVar pos name -> envLookup name dflt pos =<< getLexEnv
>            DynVar pos name -> envLookup name dflt pos =<< getDynEnv

> envLookup :: Name -> Value -> Pos -> Env.Env s Name Value -> Interpreter Value
> envLookup name dflt pos env = do ov <- lift $ Env.lookup env name dflt
>                                  case ov of
>                                      Just v -> return v
>                                      Nothing -> throwExc (Unbound pos name)

> def :: Var -> Value -> Interpreter ()
> def var val =
>     do dflt <- lift $ emptyRedirect var
>        case var of
>            LexVar pos name -> envDef name val dflt pos =<< getLexEnv
>            DynVar pos name -> envDef name val dflt pos =<< getDynEnv

> envDef :: Name -> Value -> Value -> Pos -> Env.Env s Name Value
>        -> Interpreter ()
> envDef name val dflt pos env =
>     do orv <- lift $ Env.lookup env name dflt
>        case orv of
>            Just (Redirect _ ref) ->
>                do ov <- lift $ readIORef ref
>                   case ov of
>                       Nothing -> lift (writeIORef ref (Just val))
>                       Just _ -> throwExc (ReAssignment pos name)
>            _ -> throwExc (UnAssignable pos name)

> pushScope :: (Member (Exc ItpError) r,
>               Member (State Context) r, Member (State ContDump) r,
>               SetMember Lift (Lift IO) r)
>           => LexEnv -> [(Name, Value)] -> [(Name, Value)] -> Eff r ()
> pushScope lEnv lkvs dkvs = do dEnv' <- pushFrame <$> getDynEnv <*> pure dkvs
>                               let lEnv' = pushFrame lEnv lkvs
>                               putDynEnv dEnv'
>                               putLexEnv lEnv'

> getCont :: (Member (State Context) r) => Eff r Cont
> getCont = view ctxCont <$> get

> putCont :: (Member (State Context) r) => Cont -> Eff r ()
> putCont k = modify (set ctxCont k)

> pushContFrame :: (Member (State Context) r) => (Context -> Cont) -> Eff r ()
> pushContFrame makeCont = do ctx <- get
>                             putCont (makeCont ctx)

> modifyTop :: (Member (Exc ItpError) r, Member (State Context) r)
>           => (Context -> Cont) -> Eff r ()
> modifyTop f = do k <- getCont
>                  case contCtx k of
>                      Just ctx @ (Context lEnv dEnv _) ->
>                          modify (over ctxCont (const (f ctx)))
>                      Nothing -> throwExc StackUnderflow

> pop :: (Member (Exc ItpError) r, Member (State Context) r) => Eff r ()
> pop = modifyTop (\(Context _ _ k) -> k)

> getDump :: (Member (State ContDump) r) => Eff r ContDump
> getDump = get

Abstract Machine
================

> eval :: Expr -> Interpreter Value
> eval (AST.Fn _ cases) =
>     do lEnv <- getLexEnv
>        continue $ Closure (method lEnv <$> cases)
> eval (AST.Block _ stmts) =
>     do lEnv <- getLexEnv
>        let vars = stmtBinders =<< stmts
>        let lVars = filter isLexVar vars
>        let dVars = filter isDynVar vars
>        lVals <- lift $ sequenceA (emptyRedirect <$> lVars)
>        dVals <- lift $ sequenceA (emptyRedirect <$> dVars)
>        pushScope lEnv (zip (varName <$> lVars) lVals)
>                       (zip (varName <$> dVars) dVals)
>        case stmts of
>            [stmt] -> evalStmt stmt
>            stmt:stmts' -> do pushContFrame (Stmts stmts')
>                              evalStmt stmt
> eval (AST.App _ f i args) =
>     do pushContFrame (Applicant i args)
>        eval f
> eval (AST.PrimApp _ op (arg:args)) =
>     do pushContFrame (PrimArg op [] args)
>        eval arg
> eval (AST.PrimApp _ op []) = applyPrimop op []
> eval (AST.Var var) = continue =<< lookup var
> eval (AST.Const c) = continue (evalConst c)
>     where evalConst (CST.Int _ i) = Int i
>           evalConst (CST.String _ s) = String s

> evalStmt :: Stmt -> Interpreter Value
> evalStmt (Def var expr) = do pushContFrame (Assign var)
>                              eval expr
> evalStmt (Expr expr) = eval expr

> continue :: Value -> Interpreter Value
> continue v = do k <- getCont
>                 case k of
>                     Stmts [stmt] _ ->
>                         do pop
>                            evalStmt stmt
>                     Stmts (stmt:stmts) _ ->
>                         do modifyTop (Stmts stmts)
>                            evalStmt stmt
>                     Applicant i args _ ->
>                         do modifyTop (MethodIndex v args)
>                            eval i
>                     MethodIndex f args _ ->
>                         do modifyTop (Arg f v)
>                            eval args
>                     Arg f (Int i) _ ->
>                         do pop
>                            apply f i v
>                     PrimArg op vs (arg:args) _ ->
>                         do modifyTop (PrimArg op (v:vs) args)
>                            eval arg
>                     PrimArg op vs [] _ ->
>                         do pop
>                            continue =<< applyPrimop op (reverse (v:vs))
>                     Assign var _ ->
>                         do pop
>                            def var v
>                            continue v -- QUESTION: what to return here?
>                     Halt -> return v

> apply :: Value -> Int -> Value -> Interpreter Value
> apply f i args = do f' <- unwrap f
>                     applyDirect f' i args

> applyDirect :: Value -> Int -> Value -> Interpreter Value
> applyDirect (Closure ((Method (Formals {args, ..}, _, body, lEnv)):_)) _ vs =
>     do pushScope lEnv [(varName args, vs)] []
>        eval body

> applyPrimop :: Primop -> [Value] -> Interpreter Value
> applyPrimop Ops.Tuple vs = pure $ Tuple vs
