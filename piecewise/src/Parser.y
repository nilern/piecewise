{
{-# LANGUAGE PartialTypeSignatures #-}

module Parser (expr) where
import Control.Monad.Except
import Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..),
              LexicalError(..), startPos)
import Indentation (WSLexer, readToken)
import AST (Expr(..), Const(..), Stmt(..), BlockItem(..), Pattern(..))
import Util (Pos, position, ParseError(..))
}

%name expr
%lexer { (readToken >>=) } { TokEOF _ }
%monad { WSLexer }
%tokentype { Tok }
%error { parseError }

%token
      int    { TokInt _ _ }
      ident  { TokId _ _ }
      string { TokString _ _ }
      char   { TokChar _ _ }
      op0    { TokOp _ _ Zero }
      op1    { TokOp _ _ One }
      op2    { TokOp _ _ Two }
      op3    { TokOp _ _ Three }
      op4    { TokOp _ _ Four }
      op5    { TokOp _ _ Five }
      op6    { TokOp _ _ Six }
      op7    { TokOp _ _ Seven }
      "=>"   { TokArrow _ }
      '='    { TokEq _ }
      "+="   { TokPlusEq _ }
      "->"   { TokArrow_ _ }
      '('    { TokDelim _ Paren L }
      ')'    { TokDelim _ Paren R }
      '['    { TokDelim _ Bracket L }
      ']'    { TokDelim _ Bracket R }
      '{'    { TokDelim _ Brace L }
      '}'    { TokDelim _ Brace R }
      ';'    { TokSemiColon _ }
      ','    { TokComma _ }

%%

Program : SemiColonList(Stmt) { reverse $1 }

Stmt : App '=' Expr  {% extractDef Def (reverse $1) $3 }
     | App "+=" Expr {% extractDef AugDef (reverse $1) $3 }
     | Expr          { Expr $1 }

Expr : Infix0 { $1 }

Infix0 : Infix(Infix0, op0, Infix1) { $1 }
Infix1 : Infix(Infix1, op1, Infix2) { $1 }
Infix2 : Infix(Infix2, op2, Infix3) { $1 }
Infix3 : Infix(Infix3, op3, Infix4) { $1 }
Infix4 : Infix(Infix4, op4, Infix5) { $1 }
Infix5 : Infix(Infix5, op5, Infix6) { $1 }
Infix6 : Infix(Infix6, op6, Infix7) { $1 }
Infix7 : Infix7 op7 App             { let TokOp pos name _ = $2
                                      in Call (position $1)
                                              (Var pos name)
                                              [$1, extractApp (reverse $3)] }
       | App                        { extractApp (reverse $1) }

App : Simple     { [$1] }
    | App Simple { $2 : $1 }

Simple : '(' Expr ')'                     { $2 }
       | '{' SemiColonList(BlockItem) '}' {% extractBlock (startPos $1)
                                                         (reverse $2) }
       | '[' SemiColonList(Stmt) ']'
         { Fn (startPos $1) [([PTuple (startPos $1) []], reverse $2)] }
       | ident                            { let TokId pos name = $1
                                            in Var pos name }
       | Datum                            { $1 }

Datum : Prim     { Const $1 }
      | Compound { $1 }

Prim : int    { let TokInt pos i = $1 in Int pos i }
     | string { let TokString pos cs = $1 in String pos cs }
     | char   { let TokChar pos cs = $1 in Char pos cs }

Compound : '(' ExprList ')' { Tuple (startPos $1) (reverse $2) }
         | '[' ExprList ']' { Array (startPos $1) (reverse $2) }
         | '{' ExprList '}' { Set (startPos $1) (reverse $2) }
         | '{' MapPairs '}' { Map (startPos $1) (reverse $2) }

Infix(l, op, r) : l op r { let TokOp pos name _ = $2
                           in Call (position $1) (Var pos name) [$1, $3] }
                | r      { $1 }

SemiColonList(p) : p                      { [$1] }
                 | SemiColonList(p) ';' p { $3 : $1 }

BlockItem : App "=>" Stmt { Clause (reverse $1) $3 }
          | Stmt          { Stmt $1 }

ExprList : {- empty -}     { [] }
         | Expr ','        { [$1] }
         | ExprListTwoPlus { $1 }

ExprListTwoPlus : Expr ',' Expr            { [$3, $1] }
                | ExprListTwoPlus ',' Expr { $3 : $1 }

MapPairs : "->"                        { [] }
         | Expr "->" Expr              { [($1, $3)] }
         | MapPairs ',' Expr "->" Expr { ($3, $5) : $1 }

{
parseError :: Tok -> WSLexer a
parseError tok = throwError $ ParseError (startPos tok) tok

extractBlock :: Pos -> [BlockItem] -> WSLexer Expr
extractBlock pos items @ ((Clause _ _):_) = Fn pos <$> clauses items
    where clauses ((Clause formals stmt):items) =
              do pats <- traverse exprPattern formals
                 cls <- clauses items'
                 return $ (pats, map unwrap stmts) : cls
              where (stmts, items') = span isStmt items
                    isStmt (Stmt _) = True
                    isStmt (Clause _ _) = False
                    unwrap (Stmt stmt) = stmt
          clauses [] = return []
extractBlock pos items @ ((Stmt _):_) = return $ Block pos (parseStmts items)
    where parseStmts ((Stmt stmt):stmts) = stmt : parseStmts stmts
          parseStmts [] = []

extractApp :: [Expr] -> Expr
extractApp [e] = e
extractApp (f:args) = Call (position f) f args

extractDef :: (Pattern -> Expr -> Stmt) -> [Expr] -> Expr -> WSLexer Stmt
extractDef make [pat] expr = (\p -> make p expr) <$> exprPattern pat
extractDef make (pat:formals) expr =
    do fpats <- traverse exprPattern formals
       extractDef make [pat] (Fn (position pat) [(fpats, [Expr expr])])

exprPattern :: Expr -> WSLexer Pattern
exprPattern (Var pos name) = return $ PVar pos name
exprPattern (Const (Int pos i)) = return $ PInt pos i
exprPattern (Const (String pos s)) = return $ PString pos s
exprPattern (Const (Char pos c)) = return $ PChar pos c
exprPattern (Tuple pos pats) = PTuple pos <$> traverse exprPattern pats
exprPattern (Array pos pats) = PArray pos <$> traverse exprPattern pats
exprPattern (Map pos pats) =
   PMap pos <$> traverse (\(p, q) -> (,) <$> exprPattern p <*> exprPattern q) pats
exprPattern (Set pos pats) = PSet pos <$> traverse exprPattern pats
exprPattern e = throwError $ InvalidPattern (position e) e
}
