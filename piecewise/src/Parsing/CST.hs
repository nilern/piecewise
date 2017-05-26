module Parsing.CST (Stmt(..), Expr(..), Var(..), varName, Const(..)) where
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Ops (Primop)
import Util (Name, Pos, Positioned(..))

data Stmt = Def Expr Expr
          | AugDef Expr Expr
          | Expr Expr

instance Show Stmt where
    show (Def pat val) = show pat ++ " = " ++ show val
    show (AugDef pat val) = show pat ++ " += " ++ show val
    show (Expr expr) = show expr

data Expr = Fn Pos [([Expr], Expr, Expr)]
          | Block Pos [Stmt]
          | App Pos Expr [Expr]
          | PrimApp Pos Primop [Expr]
          | Var Var
          | Const Const

instance Show Expr where
    show (Fn _ cases) = '{' : intercalate ";\n" (showCase <$> cases) ++ "}"
        where showCase (pats, cond, body) =
                  intercalate " " (show <$> pats) ++ " | " ++ show cond ++
                      " => " ++ show body
    show (Block _ stmts) = '{' : intercalate ";\n" (show <$> stmts) ++ "}"
    show (App _ f args) = '(' : intercalate " " (show <$> f:args) ++ ")"
    show (PrimApp _ op args) =
        '(' : show op ++ ' ' : intercalate " " (show <$> args) ++ ")"
    show (Var var) = show var
    show (Const c) = show c

data Var = LexVar Pos Name  -- in lexical environment (register or closure)
         | UpperLexVar Pos Name
         | GlobVar Pos Name -- in global hashtable (REPL) or exe .text section
         | DynVar Pos Name  -- in dynamic environment intertwined with stack
         deriving Eq

instance Show Var where
    show (LexVar _ name) = show name
    show (UpperLexVar _ name) = show name
    show (GlobVar _ name) = show name
    show (DynVar _ name) = '$' : show name

varName :: Var -> Name
varName (LexVar _ name) = name
varName (UpperLexVar _ name) = name
varName (GlobVar _ name) = name
varName (DynVar _ name) = name

data Const = Int Pos Int
           | Char Pos Text
           | String Pos Text
           | Keyword Pos Text

instance Show Const where
    show (Int _ i) = show i
    show (Char _ c) = '\'' : unpack c ++ "'"
    show (String _ s) = unpack s
    show (Keyword _ cs) = ':' : unpack cs

instance Positioned Stmt where
    position (Def pat _) = position pat
    position (AugDef pat _) = position pat
    position (Expr expr) = position expr

instance Positioned Expr where
    position (Fn pos _) = pos
    position (Block pos _) = pos
    position (App pos _ _) = pos
    position (PrimApp pos _ _) = pos
    position (Var v) = position v
    position (Const c) = position c

instance Positioned Var where
    position (LexVar pos _) = pos
    position (DynVar pos _) = pos

instance Positioned Const where
    position (Int pos _) = pos
    position (Char pos _) = pos
    position (String pos _) = pos
    position (Keyword pos _) = pos
