{
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
  ( Token(..)
  , lexLobot
  ) where
}

%wrapper "basic"

@int        = [\-\+]?[0-9]+
@identlower = [a-z][a-zA-Z0-9_]*
@identupper = [A-Z][a-zA-Z0-9_]*

tokens :-

  $white+     ;
  "--".*      ;
  bool        { const BOOL }
  int         { const INTTYPE }
  subset      { const SET }
  struct      { const STRUCT }
  with        { const WITH }
  ":"         { const COLON }
  ","         { const SEP }
  kind        { const KIND }
  of          { const OF }
  where       { const WHERE }
  self        { const SELF }
  "."         { const DOT }
  "="         { const EQUALS }
  "<="        { const LTE }
  "+"         { const PLUS }
  in          { const MEMBER }
  "=>"        { const IMPLIES }
  not         { const NOT }
  true        { const TRUE }
  false       { const FALSE }
  "{"         { const LBRACE }
  "}"         { const RBRACE }
  "("         { const LPAREN }
  ")"         { const RPAREN }
  @int        { INT . read }
  @identlower { IDLC }
  @identupper { IDUC }

{

data Token = IDLC String | IDUC String
           | BOOL | INTTYPE | SET
           | STRUCT | WITH | COLON | SEP
           | KIND | OF | WHERE
           | SELF | DOT | EQUALS | LTE
           | PLUS
           | MEMBER | IMPLIES | NOT
           | TRUE | FALSE
           | INT Integer
           | LBRACE | RBRACE
           | LPAREN | RPAREN
           deriving (Eq,Show)

lexLobot :: String -> [Token]
lexLobot = alexScanTokens

}
