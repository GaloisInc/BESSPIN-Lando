{
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Lobot.Lexer
Description : A lexer for the untyped AST of the Lobot sublanguage.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module provides a lexer for the Lobot sublanguage.
-}
module Lobot.Lexer
  ( TokenType(..)
  , LAYENDType(..)
  , Token(..)
  , Loc(..)
  , loc
  , LToken
  , LText
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

import Prelude hiding (lex, LT, GT)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Debug.Trace
}

%wrapper "monadUserState"

$white_no_nl = [\v\ ]
@nl          = \r?\n | \r
@nls         = ($white_no_nl*@nl)+

@int        = [0-9]+
@identlower = [a-z][a-zA-Z0-9_]*
@identupper = [A-Z][a-zA-Z0-9_]*

tokens :-

  -- the lexer's state at the beginning of a line - eat up all whitespace then
  --  handle any indentation changes
  <0> {
    "--"          { begin cmt0 } -- handle comments separately to avoid calling do_bol
    $white_no_nl+ ; -- ignore all whitespace
    \t            { do_tab } -- error on seeing a tab
    ()            { do_bol }
  }

  -- the lexer's state during a comment at the beginning of a line
  <cmt0> {
    .             ;
    @nl           { begin 0 }
  }

  <main> {
    "--".*        ; -- ignore comments
    $white_no_nl+ ; -- ignore all whitespace
    \t            { do_tab } -- error on seeing a tab

    -- a newline character switches the state to handle indentation changes
    @nl           { begin 0 }

    -- this token list must match that in Parser.y for nice error messages!
    bool          { tok BOOL }
    int           { tok INTTYPE }
    subset        { tok SET }
    struct        { tok STRUCT }
    with          { tokAnd pushLinesLayout WITH }
    ":"           { tokAnd pushLayout COLON }
    ","           { begin_popWhile popWhile_comma }
    kind          { tok KIND }
    of            { tok OF }
    where         { begin_popWhile popWhile_where }
    check         { tok CHECK }
    on            { tokAnd pushLinesLayout ON }
    that          { begin_popWhile popWhile_that }
    type          { tok TYPE }
    abstract      { tok ABSTRACT }
    "->"          { tok ARROW }
    assuming      { begin_popWhile popWhile_assmg }
    self          { tok SELF }
    return        { tok RETURN }
    "."           { tok DOT }
    "="           { tok EQUALS }
    "!="          { tok NOTEQUALS }
    "<="          { tok LTE }
    "<"           { tok LT }
    ">="          { tok GTE }
    ">"           { tok GT }
    "+"           { tok PLUS }
    "-"           { tok MINUS }
    "*"           { tok TIMES }
    "/"           { tok DIV }
    "%"           { tok MOD }
    abs           { tok ABS }
    "|"           { tok OR }
    "&"           { tok AND }
    "^"           { tok XOR }
    in            { tok IN }
    notin         { tok NOTIN }
    nonempty      { tok NONEMPTY }
    size          { tok SIZE }
    "\"           { tok DIFFERENCE }
    "=>"          { tok IMPLIES }
    "<=>"         { tok IFF }
    "!"           { tok NOT }
    true          { tok TRUE }
    false         { tok FALSE }
    "{"           { tokAnd pushLinesLayout LBRACE }
    "}"           { begin_popWhile popWhile_RBRACE }
    "("           { tokAnd pushLinesLayout LPAREN }
    ")"           { begin_popWhile popWhile_RPAREN }
    @int          { tokStr (INT . read) }
    @identlower   { tokStr IDLC }
    @identupper   { tokStr IDUC }
  }

  <popWhile_comma>  () { do_popWhile (\top -> layCtxType top /= LinesLayCtx)
                                     (tok COMMA) COMMA }

  <popWhile_where>  () { do_popWhile (\top -> layCtxType top /= TopLevelCtx)
                                     (tokAnd pushLinesLayout WHERE) WHERE }

  <popWhile_that>   () { do_popWhile (\top -> layCtxType top /= TopLevelCtx)
                                     (tokAnd pushLinesLayout THAT) THAT }

  <popWhile_assmg>  () { do_popWhile (\top -> layCtxType top /= TopLevelCtx)
                                     (tokAnd pushLinesLayout ASSUMING) ASSUMING }

  <popWhile_RBRACE> () { do_popWhile (\top -> layCtxToken top /= LBRACE)
                                     (tokAnd popLayout RBRACE) RBRACE }

  <popWhile_RPAREN> () { do_popWhile (\top -> layCtxToken top /= LPAREN)
                                     (tokAnd popLayout RPAREN) RPAREN }

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
               | CHECK
               | ON
               | THAT
               | TYPE
               | ABSTRACT
               | ARROW
               | ASSUMING
               | SELF
               | RETURN
               | DOT
               | EQUALS
               | NOTEQUALS
               | LTE
               | LT
               | GTE
               | GT
               | PLUS
               | MINUS
               | TIMES
               | MOD
               | DIV
               | ABS
               | OR
               | AND
               | XOR
               | IN
               | NOTIN
               | NONEMPTY
               | SIZE
               | DIFFERENCE
               | IMPLIES
               | IFF
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
               | LAYSEP
               | EOF
               deriving (Eq,Show)

data LAYENDType = FromNewline | FromEOF | FromOther TokenType
                  deriving (Eq,Show)

-- | We include with a token the string that generated it, for use in error messages.
data Token = Token { tokenType :: TokenType
                   , tokenString :: String }

instance Show Token where
  show (Token tktp _) = show tktp

instance Ord AlexPosn where
  compare (AlexPn i _ _) (AlexPn j _ _) = compare i j

-- | 'Loc a' is an 'a' with an 'AlexPosn', a location in a souce document
data Loc a = L { getPos :: AlexPosn, unLoc :: a }
           deriving (Ord, Functor)

instance Show a => Show (Loc a) where
  show (L (AlexPn _ l c) x) =
    show x ++ " @(" ++ show l ++ "," ++ show c ++ ")"

instance Eq a => Eq (Loc a) where
  (L _ x) == (L _ y) = x == y

-- | 'loc a b' applies the 'AlexPosn' of 'a' to 'b'
loc :: Loc a -> b -> Loc b
loc (L p _) = L p

type LToken = Loc Token
type LText  = Loc Text


-- The user state of the Alex monad

data LayoutCtxType = NormalLayCtx
                   | LinesLayCtx
                     -- ^ a context which contains a series of phrases,
                     --   separated by commas or properly indented newlines
                   | TopLevelCtx
                     -- ^ the context of a top-level declaration
                   deriving (Eq, Show)

data LayoutCtx = LayCtx { layCtxCol :: Int
                        , layCtxToken :: TokenType
                        , layCtxType :: LayoutCtxType }
                        deriving (Eq)

instance Show LayoutCtx where
  show (LayCtx c tktp laytp) =
    "LayCtx " ++ show c ++ " " ++ show tktp ++ " " ++ show laytp

topLayoutCtxType :: [LayoutCtx] -> LayoutCtxType
topLayoutCtxType [] = TopLevelCtx
topLayoutCtxType (ctx:_) = layCtxType ctx

data AlexUserState = AlexUserState { filePath :: FilePath
                                   , layoutStack :: [LayoutCtx]
                                   , popWhileLastInput :: Maybe (AlexInput, Int) }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" [] Nothing

getFilePath :: Alex FilePath
getFilePath = filePath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  oldState <- alexGetUserState
  alexSetUserState $ oldState { filePath = fp }

getLayoutStack :: Alex [LayoutCtx]
getLayoutStack = layoutStack <$> alexGetUserState

setLayoutStack :: [LayoutCtx] -> Alex ()
setLayoutStack stk = do
  oldState <- alexGetUserState
  alexSetUserState $ oldState { layoutStack = stk }

modifyLayoutStack :: ([LayoutCtx] -> [LayoutCtx]) -> Alex ()
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


-- Generating tokens

-- | Emit a token using the given input string.
tokStr :: (String -> TokenType) -> AlexAction LToken
tokStr t (p,_,_,s) len = do
  let tk = L p (Token (t (take len s)) (take len s))
  stk <- getLayoutStack
  case stk of
    [] -> pushLayoutOfType TopLevelCtx     tk
    _  -> pure ()
  pure tk

-- | Emit a token that doesn't need the input string.
tok :: TokenType -> AlexAction LToken
tok t@(LAYEND _) (p,_,_,s) len =
  pure $ L p (Token t (take len s))
tok t inp len = tokStr (const t) inp len

-- | Emit a token using 'tokStr' then execute the given action on the
-- generated token.
tokStrAnd :: (LToken -> Alex ()) -> (String -> TokenType) -> AlexAction LToken
tokStrAnd action t inp len = do
  tk <- tokStr t inp len
  action tk
  pure tk

-- | Emit a token using 'tok' then execute the given action on the
-- generated token.
tokAnd :: (LToken -> Alex ()) -> TokenType -> AlexAction LToken
tokAnd action = tokStrAnd action . const


-- Actions used in the lexer that modify the user state

-- | Push a lines layout context associated to the given token and of the
-- given type on to the stack
pushLayoutOfType :: LayoutCtxType -> LToken -> Alex ()
pushLayoutOfType layTp (L (AlexPn _ _ c) (Token tktp _)) =
  modifyLayoutStack (\stk -> (LayCtx c tktp layTp):stk)

-- | Push a normal layout context associated to the given token on to the stack
pushLayout :: LToken -> Alex ()
pushLayout = pushLayoutOfType NormalLayCtx

-- | Push a lines layout context associated to the given token on to the stack
pushLinesLayout :: LToken -> Alex ()
pushLinesLayout = pushLayoutOfType LinesLayCtx

-- | Pop the top of the layout stack.
popLayout :: LToken -> Alex ()
popLayout _ =
  modifyLayoutStack (\case
    _:stk -> stk
    stk   -> stk)

-- | Pop the top of the layout stack only if it is a context associated to
-- the given token.
popLayoutOfToken :: TokenType -> LToken -> Alex ()
popLayoutOfToken to_pop _ =
  modifyLayoutStack (\case
    (LayCtx _ tktp _):stk | tktp == to_pop -> stk
    stk                                    -> stk)

-- | Check if the top of the layout stack satisfies the given predicate. If it
-- does, pop it and execute the first given action, otherwise execute the
-- second given action.
popLayoutAndDoIf :: (LayoutCtx -> Bool) -> Alex LToken -> Alex LToken
                 -> Alex LToken
popLayoutAndDoIf cnd do_if do_else = do
  stk <- getLayoutStack
  case stk of
    top:stk' | cnd top  -> do setLayoutStack stk'
                              do_if
    _ -> do_else

-- | The action to execute at the beginning of a line - pops all layouts on
-- on the stack which are greater than the current column, emitting LAYEND
-- tokens for each, then switches back to the main lexing state. Additionally,
-- if the stack is empty, throw an error if there is any leading whitespace.
do_bol :: AlexAction LToken
do_bol inp@(AlexPn _ _ c,_,_,_) len = do
  stk <- getLayoutStack
  if null stk && c > 1 then do
    (p,_,_,_) <- alexGetInput
    alexErrorWPos p "unexpected whitespace at the start of a line"
  else popLayoutAndDoIf ((c <=) . layCtxCol)
                        (tok (LAYEND FromNewline) inp len)
                        ((addOptLAYSEP `andBegin` main) inp len)
  where addOptLAYSEP :: AlexAction LToken
        addOptLAYSEP inp' len' = do
          tp <- topLayoutCtxType <$> getLayoutStack
          case tp of
            LinesLayCtx -> tok LAYSEP inp' len'
            _ -> skip inp' len'

-- | Saves the current 'AlexAction' input, then switches to the given state.
begin_popWhile :: Int -> AlexAction LToken
begin_popWhile popWhile_state inp len = do
  setPopWhileLastInput (Just (inp, len))
  begin popWhile_state inp len

-- | Pops all layouts on the stack which satisfy the given condition, emitting
-- LAYEND (FromOther tk) tokens for each, where tk is the given token, then
-- executes the given action on the inputs saved from 'begin_popWhile' and
-- switches back to the main lexing state.
do_popWhile :: (LayoutCtx -> Bool) -> AlexAction LToken -> TokenType
            -> AlexAction LToken
do_popWhile cnd when_done tk inp len =
  popLayoutAndDoIf cnd (tok (LAYEND (FromOther tk)) inp len) $ do
    (inp', len') <- getAndClearPopWhileLastInput
    (when_done `andBegin` main) inp' len'

-- | Errors on seeing a tab.
do_tab  :: AlexAction LToken
do_tab (p,_,_,_) _ =
  alexErrorWPos p ("found a tab character! use spaces instead")


-- Some necessary utility functions adapted from:
--  https://github.com/dagit/happy-plus-alex/blob/master/src/Lexer.x

-- Note that instead of just generating a single EOF here, we first generate
-- LAYEND tokens for everything on the stack (similar to do_popWhile)
alexEOF :: Alex LToken
alexEOF = do
  (p,_,_,_) <- alexGetInput
  popLayoutAndDoIf (const True)
                   (pure $ L p (Token (LAYEND FromEOF) ""))
                   (pure $ L p (Token EOF ""))

alexMonadScanWPos :: Alex LToken
alexMonadScanWPos = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF >>= print_debug_line
    AlexError (p, _, _, s) ->
        alexErrorWPos p ("lexical error at character '" ++ take main s ++ "'")
    AlexSkip  inp' _len -> do
        alexSetInput inp'
        alexMonadScanWPos
    AlexToken inp' len action -> do
        alexSetInput inp'
        tk <- action (ignorePendingBytes inp) len
        print_debug_line tk

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


debug_lexer = False

print_debug_line :: LToken -> Alex LToken
print_debug_line tk | debug_lexer = do
                        stk <- getLayoutStack
                        trace (show tk ++ "   " ++ show stk) $ pure tk
                    | otherwise  = pure tk

}
