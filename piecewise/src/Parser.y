{
module Parser (expr) where
import Lexer (Token(TokenInt, TokenLBrace, TokenRBrace))
import AST (Exp(Int, Set))
}

%name expr
%tokentype { Token }
%error { parseError }

%token
      int { TokenInt $$ }
      '{' { TokenLBrace }
      '}' { TokenRBrace }

%%

Exp : int     { Int $1 }
    | '{' '}' { Set [] }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
