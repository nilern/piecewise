{
module Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..), Lexer, Pos(..),
              Lexer.lex, lexer) where
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
$opchar = [\.!\%&\*\+\-\/\<=>\?\\\^\|\~]

tokens :-
    $white+               ;
    "=>"                  { \pos _ -> return $ TokArrow pos }
    "+="                  { \pos _ -> return $ TokPlusEq pos }
    "="                   { \pos _ -> return $ TokEq pos }
    "->"                  { \pos _ -> return $ TokArrow_ pos }
    $digit $constituent*
        { \pos cs -> case (decimal cs) of
                         Right (i, cs') | T.null cs' -> return $ TokInt pos i
                         _ -> throwError $ MalformedNumber cs }
    $idchar $constituent* { \pos s -> return $ TokId pos s }
    $opchar $constituent* { \pos s -> TokOp pos s <$> precedence s }
    \" [^\"]* \"          { \pos s ->
                                return $ TokString pos (T.init (T.tail s)) }
    \' [^\']+ \'          { \pos s -> return $ TokChar pos (T.init (T.tail s)) }
    "("                   { \pos _ -> return $ TokDelim pos Paren L }
    ")"                   { \pos _ -> return $ TokDelim pos Paren R }
    "["                   { \pos _ -> return $ TokDelim pos Bracket L }
    "]"                   { \pos _ -> return $ TokDelim pos Bracket R }
    "{"                   { \pos _ -> return $ TokDelim pos Brace L }
    "}"                   { \pos _ -> return $ TokDelim pos Brace R }
    ";"                   { \pos _ -> return $ TokSemiColon pos }
    ","                   { \pos _ -> return $ TokComma pos }

{
type AlexInput = B.ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = B.uncons

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

data Pos = Pos Int deriving Show

data LexicalError = MalformedNumber T.Text
                  | UnprecedentedOp T.Text
                  | UnexpectedInput T.Text
                  deriving Show

type Lexer a = StateT (AlexInput, Pos) (Either LexicalError) a

lex :: Lexer a -> (AlexInput, Pos) -> Either LexicalError a
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

precedence :: T.Text -> Lexer Precedence
precedence cs | T.head cs == '|' = return Zero
              | T.head cs == '^' = return One
              | T.head cs == '&' = return Two
              | T.head cs == '=' = return Three
              | T.head cs == '!' = return Three
              | T.head cs == '<' = return Four
              | T.head cs == '>' = return Four
              | T.head cs == '+' = return Five
              | T.head cs == '-' = return Five
              | T.head cs == '*' = return Six
              | T.head cs == '/' = return Six
              | T.head cs == '%' = return Six
              | T.head cs == '.' = return Seven
              | otherwise = throwError $ UnprecedentedOp cs

readToken :: Lexer Tok
readToken = do (input, Pos pos) <- get
               case alexScan input 0 of
                   AlexEOF -> return $ TokEOF (Pos pos)
                   AlexError input' ->
                       throwError $ UnexpectedInput (decodeUtf8 input')
                   AlexSkip input' n ->
                       put (input', Pos $ pos + n) >> readToken
                   AlexToken input' n action ->
                       do put (input', Pos $ pos + n)
                          action (Pos pos) (decodeUtf8 $ B.take n input)

lexer :: (Tok -> Lexer a) -> Lexer a
lexer = (readToken >>=)
}
