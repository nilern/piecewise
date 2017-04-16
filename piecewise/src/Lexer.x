{
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Lexer (Tok(..), Delimiter(..), Side(..), Precedence(..), Pos(..),
              AlexInput, alexInput, PlainLexer, Lexer.lex, readToken,
              LexicalError(..)) where
import Data.Word (Word8)
import Data.Default
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (w2c)
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
data AlexInput = AlexInput { charPos :: Pos, inputStr :: B.ByteString }

alexInput :: B.ByteString -> AlexInput
alexInput s = AlexInput { charPos = def, inputStr = s }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput { charPos = p, inputStr = cs } =
    case B.uncons cs of
        Nothing -> Nothing
        Just (b, cs') ->
            let c  = B.w2c b
                p' = alexMove p c
            in p' `seq` cs' `seq`
               Just (b, AlexInput { charPos = p', inputStr = cs' })

alexMove :: Pos -> Char -> Pos
alexMove (Pos a l c) '\t' =
    Pos (a+1) l (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (Pos a l _) '\n' = Pos (a+1) (l+1) 1
alexMove (Pos a l c) _    = Pos (a+1) l (c+1)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

data Pos = Pos !Int !Int !Int deriving Show

instance Default Pos where
    def = Pos 0 1 1

data LexicalError = MalformedNumber T.Text
                  | UnprecedentedOp T.Text
                  | UnexpectedInput T.Text
                  | UnmatchedDelims (Maybe Delimiter) Delimiter
                  deriving Show

type PlainLexer = StateT AlexInput (Either LexicalError)

lex :: PlainLexer a -> AlexInput -> Either LexicalError a
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

precedence :: T.Text -> PlainLexer Precedence
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

readToken :: PlainLexer Tok
readToken =
    do input @ AlexInput { charPos = pos, inputStr = _ } <- get
       case alexScan input def of
           AlexEOF -> return $ TokEOF pos
           AlexError input' ->
               throwError $ UnexpectedInput $ decodeUtf8 (inputStr input')
           AlexSkip input' _ ->
               put input' >> readToken
           AlexToken input' n action ->
               do put input'
                  action pos $ decodeUtf8 $ B.take n (inputStr input)
}
