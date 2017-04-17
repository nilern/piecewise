> {-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

> module Indentation (WSLexer, runWSLexer, readToken) where
> import Control.Monad.State
> import Control.Monad.Except
> import Data.List (uncons)
> import qualified Data.Sequence as Seq
> import Data.Sequence (ViewL(..), (|>))
> import Data.Default
> import Util (Pos(..), ParseError(..))
> import qualified Lexer
> import Lexer (Lexer, Input, Tok(..), TokTag(..), Delimiter, Side(..),
>               Delimiter(..), LexicalError)

A whitespace sensitive lexer just wraps a regular lexer with some additional
state.

> type WSLexer a = StateT WSState Lexer a

As usual we need a runner function to actually do the lexing and extract a
useful result. Here we just have an error monad inside two nested state monads
and we get the final value or a lexical error out.

> runWSLexer :: WSLexer a -> WSState -> Input -> Either LexicalError a
> runWSLexer lexer wss s = Lexer.lex (evalStateT lexer wss) s

The Additional State
====================

> data WSState = WSState { tokQueue :: Seq.Seq Tok
>                        , indentStack :: [Int]
>                        , delimStack :: [Delimiter]
>                        }

> instance Default WSState where
>     def = WSState { tokQueue = def, indentStack = def, delimStack = def }

The Token Buffer
----------------

> assocTokQueue :: WSState -> Seq.Seq Tok -> WSState
> assocTokQueue s q = s { tokQueue = q }

> mapTokQueue :: (Seq.Seq Tok -> Seq.Seq Tok) -> WSState -> WSState
> mapTokQueue f s = assocTokQueue s (f (tokQueue s))

> pop :: WSLexer (Maybe Tok)
> pop = do q <- gets tokQueue
>          case Seq.viewl q of
>              tok :< q' -> modify (flip assocTokQueue q') >> return (Just tok)
>              EmptyL -> return Nothing

> push :: Tok -> WSLexer ()
> push tok = modify $ mapTokQueue (|> tok)

> shift :: WSLexer ()
> shift = lift Lexer.readToken >>= push

The Indent Stack
----------------

> assocIndentStack :: WSState -> [Int] -> WSState
> assocIndentStack s is = s { indentStack = is }

> mapIndentStack :: ([Int] -> [Int]) -> WSState -> WSState
> mapIndentStack f s = assocIndentStack s (f (indentStack s))

> indent :: Pos -> Pos -> WSLexer ()
> indent start @ (Pos _ _ startCol) end =
>     do modify $ mapIndentStack (startCol :)
>        push $ Tok (TokDelim Brace L) "{" start end

> dedent :: Pos -> Pos -> WSLexer ()
> dedent start end = do modify $ mapIndentStack tail
>                       push $ Tok (TokDelim Brace R) "}" start end

The Delimiter Stack
-------------------

> assocDelimStack :: WSState -> [Delimiter] -> WSState
> assocDelimStack st s = st { delimStack = s }

> mapDelimStack :: ([Delimiter] -> [Delimiter]) -> WSState -> WSState
> mapDelimStack f s = assocDelimStack s (f (delimStack s))

> popDelim :: WSLexer (Maybe Delimiter)
> popDelim = do delims <- gets delimStack
>               case uncons delims of
>                   Just (delim, delims') ->
>                       do modify $ flip assocDelimStack delims'
>                          return $ Just delim
>                   Nothing -> return Nothing

> pushDelim :: Delimiter -> Side -> WSLexer ()
> pushDelim ld L = modify $ mapDelimStack (ld :)
> pushDelim rd R = do ldelim <- popDelim
>                     case ldelim of
>                         Just ld | ld == rd -> return ()
>                         _ -> throwError $ UnmatchedDelims ldelim rd

> sigIndents :: WSLexer Bool
> sigIndents = gets $ null . delimStack

Reading a Token
===============

To read a token we first try to pop a token from the token buffer. If the buffer
turns out to be empty we replenish it using `shift` and try again.

> readToken :: WSLexer Tok
> readToken = pop >>= maybe (shift >> readToken) return
