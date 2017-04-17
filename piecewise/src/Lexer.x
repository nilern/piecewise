{
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..), Input,
              LexicalError,
              strToInput, Lexer, Lexer.lex, readToken, startPos) where
import Data.Function ((&))
import Data.Word (Word8)
import Data.Default
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (w2c)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Control.Monad.State
import Control.Monad.Except
import Util (Pos(..), ParseError(..))
import AST (Expr)
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
data Input = Input { charPos :: Pos, inputStr :: B.ByteString }

type AlexInput = Input

strToInput :: B.ByteString -> Input
strToInput s = Input { charPos = def, inputStr = s }

nextPos :: Pos -> Char -> Pos
nextPos (Pos a l c) '\t' =
    Pos (a+1) l (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
nextPos (Pos a l _) '\n' = Pos (a+1) (l+1) 1
nextPos (Pos a l c) _    = Pos (a+1) l (c+1)

alexGetByte :: Input -> Maybe (Word8, Input)
alexGetByte Input { charPos = p, inputStr = cs } =
    case B.uncons cs of
        Nothing -> Nothing
        Just (b, cs') ->
            let c  = B.w2c b
                p' = nextPos p c
            in p' `seq` cs' `seq`
               Just (b, Input { charPos = p', inputStr = cs' })

alexInputPrevChar :: Input -> Char
alexInputPrevChar = undefined

type LexicalError = ParseError Tok Delimiter Expr

type Lexer = StateT Input (Either LexicalError)

lex :: Lexer a -> Input -> Either LexicalError a
lex = evalStateT

data Delimiter = Paren | Bracket | Brace deriving (Show, Eq)

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

startPos :: Tok -> Pos
startPos (TokId pos _) = pos
startPos (TokOp pos _ _) = pos
startPos (TokInt pos _) = pos
startPos (TokString pos _) = pos
startPos (TokChar pos _) = pos
startPos (TokEq pos) = pos
startPos (TokPlusEq pos) = pos
startPos (TokArrow pos) = pos
startPos (TokArrow_ pos) = pos
startPos (TokDelim pos _ _) = pos
startPos (TokSemiColon pos) = pos
startPos (TokComma pos) = pos
startPos (TokEOF pos) = pos

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

-- readToken :: Lexer Tok
readToken =
    do input @ Input { charPos = pos, inputStr = _ } <- get
       case alexScan input def of
           AlexEOF -> return $ TokEOF pos
           AlexError input' ->
               throwError (input' & inputStr & decodeUtf8 & UnexpectedInput)
           AlexSkip input' _ ->
               put input' >> readToken
           AlexToken input' n action ->
               do put input'
                  action pos $ decodeUtf8 $ B.take n (inputStr input)
}
