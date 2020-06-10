{
{-|
Module      : Lobot.Core.Parser
Description : A parser for the untyped AST of the Lobot sublanguage.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module provides a parser for the Lobot sublanguage.
-}
module Lobot.Core.Parser
  ( parse )
  where

import Data.Text (Text, pack)

import Lobot.Core.Syntax
import Lobot.Core.Lexer
}

%expect 1
-- There is a shift/reduce conflict on the `ids` case of `type`, but for some
-- reason it's not a problem.

%name parse
%tokentype { Token }
%error     { (error . show) }

%token
  BOOL        { BOOL }
  INTTYPE     { INTTYPE }
  SET         { SET }
  STRUCT      { STRUCT }
  WITH        { WITH }
  ":"         { COLON }
  SEP         { SEP }
  KIND        { KIND }
  OF          { OF }
  WHERE       { WHERE }
  SELF        { SELF }
  "."         { DOT }
  "="         { EQUALS }
  "<="        { LTE }
  MEMBER      { MEMBER }
  "=>"        { IMPLIES }
  NOT         { NOT }
  TRUE        { TRUE }
  FALSE       { FALSE }
  "{"         { LBRACE }
  "}"         { RBRACE }
  "("         { LPAREN }
  ")"         { RPAREN }
  INT         { INT $$ }
  ID          { IDLC $$ }
  EID         { IDUC $$ }

%nonassoc ":"
%left     "=>"
%nonassoc "=" "<="
%nonassoc NOT
%nonassoc MEMBER
%left     "."

%%

decls :: { [KindDecl] }
decls : {- empty -}      { [] }
      | decl decls       { $1 : $2 }

decl :: { KindDecl }
decl : ID ":" KIND OF topType                       { KindDecl (pack $1) $5 [] }
     | ID ":" KIND OF topType WHERE exprs           { KindDecl (pack $1) $5 $7 }


topType :: { Type }
topType : type                         { $1 }
        | STRUCT WITH fields           { StructType $3 }

type    : BOOL                         { BoolType }
        | INTTYPE                      { IntType }
        | "{" eids "}"                 { EnumType $2 }
        | SET "{" eids "}"             { SetType $3 }
        | STRUCT                       { StructType [] }
        | STRUCT WITH "{" fields "}"   { StructType $4 }
        | ids                          { KindNames $1 }

fields :: { [(Text, Type)] }
fields : field                 { [$1] }
       | field SEP fields      { $1 : $3 }

field : ID ":" type            { (pack $1, $3) }


expr :: { Expr }
expr : lit                { LiteralExpr $1 }
     | SELF               { SelfExpr }
     | ID                 { FieldExpr SelfExpr (pack $1) }
     | expr "." ID        { FieldExpr $1 (pack $3) }
     | expr "=" expr      { EqExpr $1 $3 }
     | expr "<=" expr     { LteExpr $1 $3 }
     | expr MEMBER expr   { MemberExpr $1 $3 }
     | expr "=>" expr     { ImpliesExpr $1 $3 }
     | NOT expr           { NotExpr $2 }
     | expr ":" type      { IsInstanceExpr $1 $3 }
     | "(" expr ")"       { $2 }

exprs : expr             { [$1] }
      | expr SEP exprs   { $1 : $3 }


lit :: { Literal }
lit : TRUE                            { BoolLit True }
    | FALSE                           { BoolLit False }
    | INT                             { IntLit $1 }
    | EID                             { EnumLit (pack $1) }
    | "{" eids "}"                    { SetLit $2 }
    | STRUCT WITH "{" fieldvals "}"   { StructLit $4 }

fieldvals :: { [(Text, Literal)] }
fieldvals : {- empty -}              { [] }
          | fieldval SEP fieldvals   { $1 : $3 }

fieldval : ID "=" lit                { (pack $1, $3) }


eids : {- empty -}    { [] }
     | EID SEP eids   { pack $1 : $3 }
     | EID            { pack $1 : [] }

ids : ID   { [pack $1] }
    | ID ids        { pack $1 : $2 }


{

parseString :: String -> [KindDecl]
parseString = parse . lexLobot

}
