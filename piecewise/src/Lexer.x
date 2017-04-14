{
module Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..), Lexer, Pos(..),
              Lexer.lex, lexer) where
import Data.Char (isAlpha, isSpace, isDigit)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Control.Monad.State
import Control.Monad.Except
}

$delimiter = ["'`\(\)\[\]\{\}]
$separator = [\;\,]
$terminator = [$white $delimiter $separator]
$constituent = ~$terminator
$digit = 0-9
$idchar = [a-zA-Z\$@_]
$opchar = [!\%&\*\+\-\/\<=>\?\\\^\|\~]

tokens :-
    $white+               ;
    "=>"                  { \pos _ -> TokArrow pos }
    "+="                  { \pos _ -> TokPlusEq pos }
    "="                   { \pos _ -> TokEq pos }
    "->"                  { \pos _ -> TokArrow_ pos }
    $digit $constituent*  { \pos cs -> TokInt pos (case (decimal cs) of
                                                       Right (i, _) -> i) }
    $idchar $constituent* { TokId }
    $opchar $constituent* { \pos s -> TokOp pos s (precedence s) }
    \" [^\"]* \"          { \pos s -> TokString pos (T.init (T.tail s)) }
    \' [^\']+ \'          { \pos s -> TokChar pos (T.init (T.tail s)) }
    "("                   { \pos _ -> TokDelim pos Paren L }
    ")"                   { \pos _ -> TokDelim pos Paren R }
    "["                   { \pos _ -> TokDelim pos Bracket L }
    "]"                   { \pos _ -> TokDelim pos Bracket R }
    "{"                   { \pos _ -> TokDelim pos Brace L }
    "}"                   { \pos _ -> TokDelim pos Brace R }
    ";"                   { \pos _ -> TokSemiColon pos }
    ","                   { \pos _ -> TokComma pos }

{
type AlexInput = B.ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = B.uncons

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

data Pos = Pos Int deriving Show

type Lexer a = StateT (AlexInput, Pos) (Either String) a

lex :: Lexer a -> (AlexInput, Pos) -> Either String a
lex = evalStateT

data Delimiter = Paren | Bracket | Brace deriving Show

data Side = L | R deriving Show

data Precedence = Zero | One | Two | Three | Four | Five | Six | Seven
                deriving Show

data Tok = TokId Pos T.Text
         | TokOp Pos T.Text Precedence
         | TokInt Pos Int
         | TokString Pos T.Text
         | TokChar Pos T.Text
         | TokEq Pos
         | TokPlusEq Pos
         | TokArrow Pos
         | TokArrow_ Pos
         | TokDelim Pos Delimiter Side
         | TokSemiColon Pos
         | TokComma Pos
         | TokEOF Pos
         deriving Show

precedence :: T.Text -> Precedence
precedence cs | T.head cs == '|' = One
              | T.head cs == '^' = Two
              | T.head cs == '&' = Three
              | T.head cs == '=' = Four
              | T.head cs == '!' = Four
              | T.head cs == '<' = Five
              | T.head cs == '>' = Five
              | T.head cs == '+' = Six
              | T.head cs == '-' = Six
              | T.head cs == '*' = Seven
              | T.head cs == '/' = Seven
              | T.head cs == '%' = Seven

readToken :: Lexer Tok
readToken = do (input, Pos pos) <- get
               case alexScan input 0 of
                   AlexEOF -> return $ TokEOF $ Pos pos
                   AlexError _ -> throwError "LexicalError"
                   AlexSkip input' n ->
                       put (input', Pos $ pos + n) >> readToken
                   AlexToken input' n action ->
                       do put (input', Pos $ pos + n)
                          return $ action (Pos pos) (decodeUtf8 $B.take n input)

lexer :: (Tok -> Lexer a) -> Lexer a
lexer = (readToken >>=)
}
