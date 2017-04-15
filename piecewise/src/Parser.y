{
module Parser (expr) where
import Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..), Lexer, lexer)
import AST (Exp(..), Stmt(..), BlockItem(..), Pattern(..), exprPattern)
}

%name expr
%lexer { lexer } { TokEOF _ }
%monad { Lexer }
%tokentype { Tok }
%error { parseError }

%token
      int    { TokInt _ $$ }
      ident  { TokId _ $$ }
      string { TokString _ $$ }
      char   { TokChar _ $$ }
      op0    { TokOp _ $$ Zero }
      op1    { TokOp _ $$ One }
      op2    { TokOp _ $$ Two }
      op3    { TokOp _ $$ Three }
      op4    { TokOp _ $$ Four }
      op5    { TokOp _ $$ Five }
      op6    { TokOp _ $$ Six }
      op7    { TokOp _ $$ Seven }
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

Stmt : App '=' Expr  { extractDef Def (reverse $1) $3 }
     | App "+=" Expr { extractDef AugDef (reverse $1) $3 }
     | Expr          { Expr $1 }

Expr : Infix0 { $1 }

Infix0 : Infix0 op0 Infix1 { Call (Var $2) [$1, $3] }
       | Infix1            { $1 }

Infix1 : Infix1 op1 Infix2 { Call (Var $2) [$1, $3] }
       | Infix2            { $1 }

Infix2 : Infix2 op2 Infix3 { Call (Var $2) [$1, $3] }
       | Infix3            { $1 }

Infix3 : Infix3 op3 Infix4 { Call (Var $2) [$1, $3] }
       | Infix4            { $1 }

Infix4 : Infix4 op4 Infix5 { Call (Var $2) [$1, $3] }
       | Infix5            { $1 }

Infix5 : Infix5 op5 Infix6 { Call (Var $2) [$1, $3] }
       | Infix6            { $1 }

Infix6 : Infix6 op6 Infix7 { Call (Var $2) [$1, $3] }
       | Infix7            { $1 }

Infix7 : Infix7 op7 App { Call (Var $2) [$1, extractApp (reverse $3)] }
       | App            { extractApp (reverse $1) }

App : Simple     { [$1] }
    | App Simple { $2 : $1 }

Simple : '(' Expr ')'                     { $2 }
       | '{' SemiColonList(BlockItem) '}' { extractBlock (reverse $2) }
       | '[' SemiColonList(Stmt) ']'      { Fn [([PTuple []], reverse $2)] }
       | ident                            { Var $1 }
       | Datum                            { $1 }

Datum : Prim     { $1 }
      | Compound { $1 }

Prim : int    { Int $1 }
     | string { String $1}
     | char   { Char $1}

Compound : '(' ExprList ')' { Tuple (reverse $2) }
         | '[' ExprList ']' { Array (reverse $2) }
         | '{' ExprList '}' { Set (reverse $2) }
         | '{' MapPairs '}' { Map (reverse $2) }

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
parseError :: Tok -> a
parseError _ = error "Parse error"

extractBlock :: [BlockItem] -> Exp
extractBlock items @ ((Clause _ _):_) = Fn $ clauses items
    where clauses ((Clause formals stmt):items) =
              (map exprPattern formals, stmt : map unwrap stmts)
              : clauses items'
              where (stmts, items') = span isStmt items
                    isStmt (Stmt _) = True
                    isStmt (Clause _ _) = False
                    unwrap (Stmt stmt) = stmt
          clauses [] = []
extractBlock items @ ((Stmt _):_) = Block $ parseStmts items
    where parseStmts ((Stmt stmt):stmts) = stmt : parseStmts stmts
          parseStmts [] = []

extractApp :: [Exp] -> Exp
extractApp [e] = e
extractApp (f:args) = Call f args

extractDef :: (Pattern -> Exp -> Stmt) -> [Exp] -> Exp -> Stmt
extractDef make [pat] expr = make (exprPattern pat) expr
extractDef make (pat:formals) expr =
    extractDef make [pat] $ Fn [(map exprPattern formals, [Expr expr])]
}
