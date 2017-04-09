{
module Parser (expr) where
import Lexer (Tok(TokInt, TokEq, TokPlusEq, TokArrow,
                    TokDelim, TokSemiColon, TokComma),
              Delimiter(Brace), Side(L, R))
import AST (Exp(Fn, Block, Int, Set),
            Stmt(Def, AugDef, Expr),
            BlockItem(Clause, Stmt))
}

%name expr
%tokentype { Tok }
%error { parseError }

%token
      int  { TokInt $$ }
      "=>" { TokArrow }
      '='  { TokEq }
      "+=" { TokPlusEq }
      '{'  { TokDelim Brace L }
      '}'  { TokDelim Brace R }
      ';'  { TokSemiColon }
      ','  { TokComma }

%%

Exp : Block { $1 }

Block : '{' BlockItemList '}' { parseBlock (reverse $2) }

BlockItemList : BlockItem                   { [$1] }
              | BlockItemList ';' BlockItem { $3 : $1 }

BlockItem : Formals "=>" Stmt { Clause (reverse $1) $3 }
          | Stmt              { Stmt $1 }

Stmt : Pattern '=' Datum  { Def $1 $3 }
     | Pattern "+=" Datum { AugDef $1 $3 }
     | Datum              { Expr $1 }

Formals : Pattern         { [$1] }
        | Formals Pattern { $2 : $1 }

Pattern : Datum { $1 }

Datum : int              { Int $1 }
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
}
