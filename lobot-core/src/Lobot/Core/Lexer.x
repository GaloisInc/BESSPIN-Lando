{
{-# LANGUAGE DeriveFunctor #-}

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
}

%wrapper "monadUserState"

@int        = [\-\+]?[0-9]+
@identlower = [a-z][a-zA-Z0-9_]*
@identupper = [A-Z][a-zA-Z0-9_]*

tokens :-

  $white+     ;
  "--".*      ;
  -- This token list must match that in Parser.y!
  bool        { tok BOOL }
  int         { tok INTTYPE }
  subset      { tok SET }
  struct      { tok STRUCT }
  with        { tok WITH }
  ":"         { tok COLON }
  ","         { tok COMMA }
  kind        { tok KIND }
  of          { tok OF }
  where       { tok WHERE }
  self        { tok SELF }
  "."         { tok DOT }
  "="         { tok EQUALS }
  "<="        { tok LTE }
  "+"         { tok PLUS }
  in          { tok IN }
  "=>"        { tok IMPLIES }
  not         { tok NOT }
  true        { tok TRUE }
  false       { tok FALSE }
  "{"         { tok LBRACE }
  "}"         { tok RBRACE }
  "("         { tok LPAREN }
  ")"         { tok RPAREN }
  @int        { tokStr (INT . read) }
  @identlower { tokStr IDLC }
  @identupper { tokStr IDUC }

{

data TokenType = EOF
               | BOOL
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
               deriving (Eq,Show)

data Token = Token { tokenType :: TokenType
                   , tokenString :: String }

data Loc a = L { getPos :: AlexPosn, unLoc :: a }
           deriving (Show, Functor)

loc :: Loc a -> b -> Loc b
loc (L p _) = L p

type LToken = Loc Token

tokStr :: (String -> TokenType) -> AlexAction LToken
tokStr t (p,_,_,s) len = pure $ L p (Token (t (take len s)) (take len s))

tok :: TokenType -> AlexAction LToken
tok = tokStr . const


-- The user state of the Alex monad

data AlexUserState = AlexUserState { filePath :: FilePath
                                   , layoutStack :: [Int] }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" []

getFilePath :: Alex FilePath
getFilePath = filePath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  oldState <- alexGetUserState
  alexSetUserState $ oldState { filePath = fp }

-- Some necessary utility functions from:
--  https://github.com/dagit/happy-plus-alex/blob/master/src/Lexer.x

alexEOF :: Alex LToken
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ L p (Token EOF "")

alexMonadScanWPos :: Alex LToken
alexMonadScanWPos = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexErrorWPos p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' _len -> do
        alexSetInput inp'
        alexMonadScanWPos
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

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
