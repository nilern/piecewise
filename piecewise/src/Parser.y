{
module Parser (expr) where
import Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..))
import AST (Exp(..), Stmt(..), BlockItem(..))
}

%name expr
%tokentype { Tok }
%error { parseError }

%token
      int    { TokInt $$ }
      ident  { TokId $$ }
      string { TokString $$ }
      char   { TokChar $$ }
      op1    { TokOp $$ One }
      op2    { TokOp $$ Two }
      op3    { TokOp $$ Three }
      op4    { TokOp $$ Four }
      op5    { TokOp $$ Five }
      op6    { TokOp $$ Six }
      op7    { TokOp $$ Seven }
      "=>"   { TokArrow }
      '='    { TokEq }
      "+="   { TokPlusEq }
      "->"   { TokArrow_ }
      '('    { TokDelim Paren L }
      ')'    { TokDelim Paren R }
      '['    { TokDelim Bracket L }
      ']'    { TokDelim Bracket R }
      '{'    { TokDelim Brace L }
      '}'    { TokDelim Brace R }
      ';'    { TokSemiColon }
      ','    { TokComma }

%%

Program : StmtList { reverse $1 }

StmtList : Stmt              { [$1] }
         | StmtList ';' Stmt { $3 : $1 }

Stmt : App '=' Expr  { extractDef Def (reverse $1) $3 }
     | App "+=" Expr { extractDef AugDef (reverse $1) $3 }
     | Expr          { Expr $1 }

Expr : Infix1 { $1 }

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

Simple : '(' Expr ')'          { $2 }
       | '{' BlockItemList '}' { extractBlock (reverse $2) }
       | '[' StmtList ']'      { Fn [([Tuple []], reverse $2)] }
       | ident                 { Var $1 }
       | Datum                 { $1 }

Datum : int              { Int $1 }
      | string           { String $1}
      | char             { Char $1}
      | '(' ExprList ')' { Tuple (reverse $2) }
      | '[' ExprList ']' { Array (reverse $2) }
      | '{' ExprList '}' { Set (reverse $2) }
      | '{' MapPairs '}' { Map (reverse $2) }

BlockItemList : BlockItem                   { [$1] }
              | BlockItemList ';' BlockItem { $3 : $1 }

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
parseError :: [Tok] -> a
parseError _ = error "Parse error"

extractBlock :: [BlockItem] -> Exp
extractBlock items @ ((Clause _ _):_) = Fn $ clauses items
    where clauses ((Clause formals stmt):items) =
              (formals, stmt : map unwrap stmts) : clauses items'
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

extractDef :: (Exp -> Exp -> Stmt) -> [Exp] -> Exp -> Stmt
extractDef make [pat] expr = make pat expr
extractDef make (pat:formals) expr = make pat $ Fn [(formals, [Expr expr])]
}
