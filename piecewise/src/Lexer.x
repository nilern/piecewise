{
{-# LANGUAGE OverloadedStrings #-}

module Lexer (TokTag(..), Tok(..), Delimiter(..), Side(..), Precedence(..),
              Input, LexicalError, charPos,
              strToInput, startPos, Lexer, Lexer.lex, readToken) where
import Data.Function ((&))
import Data.Word (Word8)
import Data.Default
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (w2c)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.State
import Control.Monad.Except
import Util (Pos(..), nextPos, ParseError(..))
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
    \# [^\n]*             ;
    "=>"                  { \cs p q -> return $ Tok TokArrow cs p q }
    "+="                  { \cs p q -> return $ Tok TokPlusEq cs p q }
    "="                   { \cs p q -> return $ Tok TokEq cs p q }
    "->"                  { \cs p q -> return $ Tok TokArrow_ cs p q }
    $digit $constituent*  { \cs p q -> return $ Tok TokInt cs p q }
    $idchar $constituent* { \cs p q -> return $ Tok TokId cs p q }
    $opchar $constituent*
        { \cs p q -> do { prec <- precedence cs;
                          return $ Tok (TokOp prec) cs p q } }
    \" [^\"]* \"
        { \cs p q -> return $ Tok TokString (T.init (T.tail cs)) p q }
    \' [^\']+ \'
        { \cs p q -> return $ Tok TokChar (T.init (T.tail cs)) p q }
    "(" { \cs p q -> return $ Tok (TokDelim Paren L) cs p q }
    ")" { \cs p q -> return $ Tok (TokDelim Paren R) cs p q }
    "[" { \cs p q -> return $ Tok (TokDelim Bracket L) cs p q }
    "]" { \cs p q -> return $ Tok (TokDelim Bracket R) cs p q }
    "{" { \cs p q -> return $ Tok (TokDelim Brace L) cs p q }
    "}" { \cs p q -> return $ Tok (TokDelim Brace R) cs p q }
    ";" { \cs p q -> return $ Tok TokSemiColon cs p q }
    "," { \cs p q -> return $ Tok TokComma cs p q }

{
data Input = Input { charPos :: Pos, inputStr :: B.ByteString }

type AlexInput = Input

strToInput :: B.ByteString -> Input
strToInput s = Input { charPos = def, inputStr = s }

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

data TokTag = TokId
            | TokOp Precedence
            | TokInt
            | TokString
            | TokChar
            | TokEq
            | TokPlusEq
            | TokArrow
            | TokArrow_
            | TokDelim Delimiter Side
            | TokSemiColon
            | TokComma
            | TokEOF
            deriving Show

data Tok = Tok TokTag T.Text Pos Pos deriving Show

startPos :: Tok -> Pos
startPos (Tok _ _ pos _) = pos

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
readToken =
    do input @ Input { charPos = pos, inputStr = s } <- get
       case alexScan input def of
           AlexEOF ->
               return (Tok TokEOF "" pos pos)
           AlexError Input { charPos = _, inputStr = s' } ->
               throwError (s' & decodeUtf8 & UnexpectedInput)
           AlexSkip input' _ ->
               put input' >> readToken
           AlexToken input' @ Input { charPos = pos', inputStr = _ } n action ->
               put input' >> action (s & B.take n & decodeUtf8) pos pos'
}
