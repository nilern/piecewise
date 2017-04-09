{
module Parser (expr) where
import Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..))
import AST (Exp(..), Stmt(..), BlockItem(..))
}

%name expr
%tokentype { Tok }
%error { parseError }

%token
      int   { TokInt $$ }
      ident { TokId $$ }
      op1   { TokOp $$ One }
      op2   { TokOp $$ Two }
      op3   { TokOp $$ Three }
      op4   { TokOp $$ Four }
      op5   { TokOp $$ Five }
      op6   { TokOp $$ Six }
      op7   { TokOp $$ Seven }
      "=>"  { TokArrow }
      '='   { TokEq }
      "+="  { TokPlusEq }
      '{'   { TokDelim Brace L }
      '}'   { TokDelim Brace R }
      ';'   { TokSemiColon }
      ','   { TokComma }

%%

Exp : Block { $1 }

Block : '{' BlockItemList '}' { parseBlock (reverse $2) }

BlockItemList : BlockItem                   { [$1] }
              | BlockItemList ';' BlockItem { $3 : $1 }

BlockItem : App "=>" Stmt { Clause (reverse $1) $3 }
          | Stmt          { Stmt $1 }

Stmt : Simple '=' Infix1  { Def $1 $3 }
     | Simple "+=" Infix1 { AugDef $1 $3 }
     | Infix1             { Expr $1 }

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

Infix7 : Infix7 op7 App { Call (Var $2) [$1, parseApp (reverse $3)] }
       | App            { parseApp (reverse $1) }

App : Simple     { [$1] }
    | App Simple { $2 : $1 }

Simple : ident { Var $1 }
       | Datum { $1 }

Datum : int { Int $1 }
      -- | '{' CommaSep '}' { Set (reverse $2) }

-- CommaSep : {- empty -}      { [] }
--          | Exp              { [$1] }
--          | CommaSep ',' Exp { $3 : $1 }

{
parseError :: [Tok] -> a
parseError _ = error "Parse error"

parseBlock :: [BlockItem] -> Exp
parseBlock items @ ((Clause _ _):_) = Fn $ clauses items
    where clauses ((Clause formals stmt):items) =
              (formals, stmt : map unwrap stmts) : clauses items'
              where (stmts, items') = span isStmt items
                    isStmt (Stmt _) = True
                    isStmt (Clause _ _) = False
                    unwrap (Stmt stmt) = stmt
          clauses [] = []
parseBlock items @ ((Stmt _):_) = Block $ parseStmts items
    where parseStmts ((Stmt stmt):stmts) = stmt : parseStmts stmts
          parseStmts [] = []

parseApp :: [Exp] -> Exp
parseApp [e] = e
parseApp (f:args) = Call f args
}
