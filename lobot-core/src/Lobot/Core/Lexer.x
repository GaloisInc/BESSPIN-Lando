{
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Lobot.Core.Lexer
Description : A lexer for the untyped AST of the Lobot sublanguage.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module provides a lexer for the Lobot sublanguage.
-}
module Lobot.Core.Lexer
  ( TokenType(..)
  , LAYENDType(..)
  , Token(..)
  , Loc(..)
  , loc
  , LToken
  , AlexPosn(..)
  , Alex(..)
  , AlexState(..)
  , AlexUserState(..)
  , Byte
  , runAlexOnFile
  , alexMonadScanWPos
  , errorPrefix
  , alexErrorWPos
  , lexer
  ) where

import Prelude hiding (lex)
import Data.Maybe (fromJust)
import Debug.Trace
}

%wrapper "monadUserState"

$white_no_nl = [\v\ ]
@nl          = \r?\n | \r
@nls         = ($white_no_nl*@nl)+

@int        = [\-\+]?[0-9]+
@identlower = [a-z][a-zA-Z0-9_]*
@identupper = [A-Z][a-zA-Z0-9_]*

tokens :-
  -- always ignore comments
  
  
  -- the lexer's state at the beginning of a line - eat up all whitespace then
  --  handle any indentation changes
  <0> {
    "--".*@nl?     ;
    $white_no_nl+  ;
    ()             { do_bol }
  }
  
  <main> {
    "--".*        ;
    $white_no_nl+ ;
    -- a newline character switches the state to handle indentation changes
    @nl           { begin 0 }
    -- this token list must match that in Parser.y for nice error messages!
    bool          { tok BOOL }
    int           { tok INTTYPE }
    subset        { tok SET }
    struct        { tok STRUCT }
    with          { tokAnd pushLayout WITH }
    ":"           { tokAnd pushLayout COLON }
    ","           { tokAnd (popLayout COLON) COMMA }
    kind          { tok KIND }
    of            { tok OF }
    where         { begin_popWhile popWhile_where }
    self          { tok SELF }
    "."           { tok DOT }
    "="           { tok EQUALS }
    "<="          { tok LTE }
    "+"           { tok PLUS }
    in            { tok IN }
    "=>"          { tok IMPLIES }
    not           { tok NOT }
    true          { tok TRUE }
    false         { tok FALSE }
    "{"           { tokAnd (popLayout WITH >> pushLayout) LBRACE }
    "}"           { begin_popWhile popWhile_RBRACE }
    "("           { tok LPAREN }
    ")"           { tok RPAREN }
    @int          { tokStr (INT . read) }
    @identlower   { tokStrAnd pushLayoutIfTopLevel IDLC }
    @identupper   { tokStr IDUC }
  }

  <popWhile_where>  () { do_popWhile (not . isIDLC . snd)
                                     (tokAnd pushLayout WHERE) }
  
  <popWhile_RBRACE> () { do_popWhile (not . (== LBRACE) .snd)
                                     (tokAnd pushLayout RBRACE) }

{

data TokenType = BOOL
               | INTTYPE
               | SET
               | STRUCT
               | WITH
               | COLON
               | COMMA
               | KIND
               | OF
               | WHERE
               | SELF
               | DOT
               | EQUALS
               | LTE
               | PLUS
               | IN
               | IMPLIES
               | NOT
               | TRUE
               | FALSE
               | LBRACE
               | RBRACE
               | LPAREN
               | RPAREN
               | INT Integer
               | IDLC String
               | IDUC String
               | LAYEND LAYENDType
               | EOF
               deriving (Eq,Show)

data LAYENDType = FromNewline | FromEOF | FromOther
                  deriving (Eq,Show)

isIDLC :: TokenType -> Bool
isIDLC (IDLC _) = True
isIDLC _ = False

data Token = Token { tokenType :: TokenType
                   , tokenString :: String }

instance Show Token where
  show (Token tktp _) = show tktp

data Loc a = L { getPos :: AlexPosn, unLoc :: a }
           deriving (Functor)

instance Show a => Show (Loc a) where
  show (L (AlexPn _ l c) x) =
    show x ++ " @(" ++ show l ++ "," ++ show c ++ ")"

loc :: Loc a -> b -> Loc b
loc (L p _) = L p

type LToken = Loc Token

tokStr :: (String -> TokenType) -> AlexAction LToken
tokStr t (p,_,_,s) len = pure $ L p (Token (t (take len s)) (take len s))

tokStrAnd :: (LToken -> Alex ()) -> (String -> TokenType) -> AlexAction LToken
tokStrAnd action t inp len = do
  tk <- tokStr t inp len
  action tk
  pure tk

tok :: TokenType -> AlexAction LToken
tok = tokStr . const

tokAnd :: (LToken -> Alex ()) -> TokenType -> AlexAction LToken
tokAnd action = tokStrAnd action . const

-- The user state of the Alex monad

data AlexUserState = AlexUserState { filePath :: FilePath
                                   , layoutStack :: [(Int,TokenType)]
                                   , popWhileLastInput :: Maybe (AlexInput, Int) }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" [] Nothing

getFilePath :: Alex FilePath
getFilePath = filePath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  oldState <- alexGetUserState
  alexSetUserState $ oldState { filePath = fp }

getLayoutStack :: Alex [(Int,TokenType)]
getLayoutStack = layoutStack <$> alexGetUserState

setLayoutStack :: [(Int,TokenType)] -> Alex ()
setLayoutStack stk = do
  oldState <- alexGetUserState
  alexSetUserState $ oldState { layoutStack = stk }

modifyLayoutStack :: ([(Int,TokenType)] -> [(Int,TokenType)]) -> Alex ()
modifyLayoutStack f = do
  stk <- getLayoutStack
  setLayoutStack (f stk)

setPopWhileLastInput :: Maybe (AlexInput, Int) -> Alex ()
setPopWhileLastInput mb_inpLen = do
  oldState <- alexGetUserState
  alexSetUserState $ oldState { popWhileLastInput = mb_inpLen }

getAndClearPopWhileLastInput :: Alex (AlexInput, Int)
getAndClearPopWhileLastInput = do
  (inp, len) <- fromJust . popWhileLastInput <$> alexGetUserState
  setPopWhileLastInput Nothing
  pure (inp, len)

-- ...

pushLayout :: LToken -> Alex ()
pushLayout (L (AlexPn _ _ c) (Token tktp _)) =
  modifyLayoutStack (\stk -> (c,tktp) : stk)

pushLayoutIfTopLevel :: LToken -> Alex ()
pushLayoutIfTopLevel tk = do
  stk <- getLayoutStack
  case stk of
    [] -> pushLayout tk
    _  -> pure ()

popLayout :: TokenType -> LToken -> Alex ()
popLayout to_pop _ =
  modifyLayoutStack (\case
    (_,tktp):stk | tktp == to_pop -> stk
    stk                           -> stk)

tryPopLayout :: ((Int,TokenType) -> Bool) -> Alex LToken -> Alex LToken -> Alex LToken
tryPopLayout cnd do_if do_else = do
  stk <- getLayoutStack
  case stk of
    top:stk' | cnd top  -> do setLayoutStack stk'
                              do_if
    _ -> do_else

do_bol :: AlexAction LToken
do_bol inp@(AlexPn _ _ c,_,_,_) len = do
  stk <- getLayoutStack
  if null stk && c > 1 then do
    (p,_,_,_) <- alexGetInput
    alexErrorWPos p "unexpected whitespace at the start of a line"
  else tryPopLayout ((c <=) . fst) (tok (LAYEND FromNewline) inp len)
                                   (begin main inp len)

begin_popWhile :: Int -> AlexAction LToken
begin_popWhile popWhile_state inp len = do
  setPopWhileLastInput (Just (inp, len))
  begin popWhile_state inp len

do_popWhile :: ((Int,TokenType) -> Bool) -> AlexAction LToken -> AlexAction LToken
do_popWhile cnd when_done inp len =
  tryPopLayout cnd (tok (LAYEND FromOther) inp len) $ do
    (inp', len') <- getAndClearPopWhileLastInput
    (when_done `andBegin` main) inp' len'

-- Some necessary utility functions adapted from:
--  https://github.com/dagit/happy-plus-alex/blob/master/src/Lexer.x

debug_lexer = False

-- ...
alexEOF :: Alex LToken
alexEOF = do
  (p,_,_,_) <- alexGetInput
  tryPopLayout (const True) (pure $ L p (Token (LAYEND FromEOF) ""))
                            (pure $ L p (Token EOF ""))

alexMonadScanWPos :: Alex LToken
alexMonadScanWPos = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  tk <- case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexErrorWPos p ("lexical error at character '" ++ take main s ++ "'")
    AlexSkip  inp' _len -> do
        alexSetInput inp'
        alexMonadScanWPos
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len
  if debug_lexer then do
    stk <- getLayoutStack
    trace (show tk ++ "   " ++ show stk) (pure tk)
  else pure tk

errorPrefix :: FilePath -> AlexPosn -> String
errorPrefix fp (AlexPn _ l c) = fp ++ ":" ++ show l ++ ":" ++ show c ++ ":"

alexErrorWPos :: AlexPosn -> String -> Alex a
alexErrorWPos p msg = do
  fp <- getFilePath
  alexError (errorPrefix fp p ++ " " ++ msg)

runAlexOnFile :: Alex a -> FilePath -> String -> Either String a
runAlexOnFile a fp input = runAlex input (setFilePath fp >> a)

lexer :: (LToken -> Alex a) -> Alex a
lexer = (alexMonadScanWPos >>=)

}
