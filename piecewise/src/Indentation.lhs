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
>               Delimiter(..), LexicalError, charPos)

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
>                        , prevEnd :: Maybe Pos
>                        , indentStack :: [Int]
>                        , delimStack :: [Delimiter] }

> instance Default WSState where
>     def = WSState { tokQueue = def
>                   , prevEnd = def
>                   , indentStack = def
>                   , delimStack = def }

> assocPrevEnd :: WSState -> Pos -> WSState
> assocPrevEnd s pos = s { prevEnd = Just pos }

The Token Buffer
----------------

> assocTokQueue :: WSState -> Seq.Seq Tok -> WSState
> assocTokQueue s q = s { tokQueue = q }

> mapTokQueue :: (Seq.Seq Tok -> Seq.Seq Tok) -> WSState -> WSState
> mapTokQueue f s = assocTokQueue s (f (tokQueue s))

> pop :: WSLexer (Maybe Tok)
> pop = do q <- gets tokQueue
>          case Seq.viewl q of
>              tok @ (Tok _ _ _ end) :< q' ->
>                  do modify (flip assocPrevEnd end . flip assocTokQueue q')
>                     return (Just tok)
>              EmptyL -> return Nothing

> push :: Tok -> WSLexer ()
> push tok = modify $ mapTokQueue (|> tok)

The Indent Stack
----------------

> assocIndentStack :: WSState -> [Int] -> WSState
> assocIndentStack s is = s { indentStack = is }

> mapIndentStack :: ([Int] -> [Int]) -> WSState -> WSState
> mapIndentStack f s = assocIndentStack s (f (indentStack s))

> currIndent :: WSState -> Int
> currIndent WSState { indentStack = i : _ } = i
> currIndent WSState { indentStack = [] } = 1

> newline :: Pos -> Pos -> WSLexer ()
> newline start end = push $ Tok TokSemiColon ";" start end

> indent :: Pos -> Pos -> WSLexer ()
> indent start end @ (Pos _ _ endCol) =
>     do modify $ mapIndentStack (endCol :)
>        push $ Tok (TokDelim Brace L) "{" start end

> dedent :: Pos -> Pos -> WSLexer ()
> dedent start end = do modify $ mapIndentStack tail
>                       push $ Tok (TokDelim Brace R) "}" start end

> dedentDownTo :: Int -> Pos -> Pos -> WSLexer ()
> dedentDownTo dest start end =
>     do curr <- gets currIndent
>        case compare curr dest of
>            EQ -> return ()
>            GT -> dedent start end >> dedentDownTo dest start end
>            LT -> do pos <- lift (gets charPos)
>                     throwError $ WildDedent pos dest curr

The Delimiter Stack
-------------------

> assocDelimStack :: WSState -> [Delimiter] -> WSState
> assocDelimStack st s = st { delimStack = s }

> mapDelimStack :: ([Delimiter] -> [Delimiter]) -> WSState -> WSState
> mapDelimStack f s = assocDelimStack s (f (delimStack s))

> sigIndents :: WSLexer Bool
> sigIndents = gets $ null . delimStack

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

> delimiter :: Tok -> WSLexer ()
> delimiter (Tok (TokDelim delim side) _ _ _) = pushDelim delim side
> delimiter _ = return ()

Reading a Token
===============

> enqueueWsTokens :: Pos -> WSLexer ()
> enqueueWsTokens start @ (Pos _ line col) =
>     do doIt <- sigIndents
>        if doIt
>        then do st <- get
>                case prevEnd st of
>                    Just (pEnd @ (Pos _ pLine _)) | line > pLine ->
>                        case compare col (currIndent st) of
>                            EQ -> newline pEnd start
>                            GT -> indent pEnd start
>                            LT -> do dedentDownTo col pEnd start
>                                     newline pEnd start
>                                     return ()
>                    _ -> return ()
>        else return ()

> shift :: WSLexer ()
> shift = do tok <- lift Lexer.readToken
>            case tok of
>                Tok TokEOF _ _ _ -> do pEnd <- gets (maybe def id . prevEnd)
>                                       dedentDownTo 1 pEnd pEnd
>                Tok _ _ start _ -> do enqueueWsTokens start
>                                      delimiter tok
>            push tok

To read a token we first try to pop a token from the token buffer. If the buffer
turns out to be empty we replenish it using `shift` and try again.

> readToken :: WSLexer Tok
> readToken = pop >>= maybe (shift >> readToken) return
