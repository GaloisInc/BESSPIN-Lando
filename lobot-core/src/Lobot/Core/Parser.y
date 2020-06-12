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
  ( parseDecls )
  where

import Data.Text (Text, pack)
import Data.List (intercalate)

import Lobot.Core.Syntax
import Lobot.Core.Lexer
}

%expect 1
-- There is a shift/reduce conflict on the `ids` case of `type`, but for some
-- reason it's not a problem.

%name parseDeclsM decls

%tokentype { TokenWPos }
%errorhandlertype explist
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexer } { Token EOF _ _ }

%token
  -- This token list must match that in Lexer.y!
  -- This list is what's used for pretty-printing expected tokens :(
  'bool'      { Token BOOL _ _ }
  'int'       { Token INTTYPE _ _ }
  'subset'    { Token SET _ _ }
  'struct'    { Token STRUCT _ _ }
  'with'      { Token WITH _ _ }
  ':'         { Token COLON _ _ }
  ','         { Token COMMA _ _ }
  'kind'      { Token KIND _ _ }
  'of'        { Token OF _ _ }
  'where'     { Token WHERE _ _ }
  'self'      { Token SELF _ _ }
  '.'         { Token DOT _ _ }
  '='         { Token EQUALS _ _ }
  '<='        { Token LTE _ _ }
  '+'         { Token PLUS _ _ }
  'in'        { Token IN _ _ }
  '=>'        { Token IMPLIES _ _ }
  'not'       { Token NOT _ _ }
  'true'      { Token TRUE _ _ }
  'false'     { Token FALSE _ _ }
  '{'         { Token LBRACE _ _ }
  '}'         { Token RBRACE _ _ }
  '('         { Token LPAREN _ _ }
  ')'         { Token RPAREN _ _ }
  int         { Token (INT $$) _ _ }
  ident       { Token (IDLC $$) _ _ }
  enumIdent   { Token (IDUC $$) _ _ }

%nonassoc ':'
%left     '=>'
%nonassoc '=' '<='
%left     '+'
%nonassoc 'not'
%nonassoc 'in'
%left     '.'

%%

decls :: { [KindDecl] }
decls : {- empty -}      { [] }
      | decl decls       { $1 : $2 }

decl :: { KindDecl }
decl : ident ':' 'kind' 'of' topType                 { KindDecl (pack $1) $5 [] }
     | ident ':' 'kind' 'of' topType 'where' exprs   { KindDecl (pack $1) $5 $7 }


topType :: { Type }
topType : type                             { $1 }
        | 'struct' 'with' fields           { StructType $3 }

type    : 'bool'                           { BoolType }
        | 'int'                            { IntType }
        | '{' enumIdents '}'               { EnumType $2 }
        | 'subset' '{' enumIdents '}'      { SetType $3 }
        | 'struct'                         { StructType [] }
        | 'struct' 'with' '{' fields '}'   { StructType $4 }
        | idents                           { KindNames $1 }

fields :: { [(Text, Type)] }
fields : field                 { [$1] }
       | field ',' fields      { $1 : $3 }

field : ident ':' type         { (pack $1, $3) }


expr :: { Expr }
expr : lit                   { LiteralExpr $1 }
     | 'self'                { SelfExpr }
     | ident                 { FieldExpr SelfExpr (pack $1) }
     | expr '.' ident        { FieldExpr $1 (pack $3) }
     | expr '=' expr         { EqExpr $1 $3 }
     | expr '<=' expr        { LteExpr $1 $3 }
     | expr '+' expr         { PlusExpr $1 $3 }
     | expr 'in' expr        { MemberExpr $1 $3 }
     | expr '=>' expr        { ImpliesExpr $1 $3 }
     | 'not' expr            { NotExpr $2 }
     | ident '(' exprs0 ')'  { ApplyExpr (pack $1) $3 }
     | expr ':' idents       { IsInstanceExpr $1 (KindNames $3) }
     | '(' expr ')'          { $2 }

exprs0 : {- empty -} { [] }
       | exprs       { $1 }

exprs : expr             { [$1] }
      | expr ',' exprs   { $1 : $3 }


lit :: { Literal }
lit : 'true'                              { BoolLit True }
    | 'false'                             { BoolLit False }
    | int                                 { IntLit $1 }
    | enumIdent                           { EnumLit (pack $1) }
    | '{' enumIdents '}'                  { SetLit $2 }
    | 'struct' 'with' '{' fieldvals '}'   { StructLit $4 }

fieldvals :: { [(Text, Literal)] }
fieldvals : {- empty -}              { [] }
          | fieldval ',' fieldvals   { $1 : $3 }

fieldval : ident '=' lit             { (pack $1, $3) }


enumIdents : {- empty -}    { [] }
           | enumIdent ',' enumIdents   { pack $1 : $3 }
           | enumIdent                  { pack $1 : [] }

idents : ident          { [pack $1] }
       | ident idents   { pack $1 : $2 }


{

parseError :: (TokenWPos, [String]) -> Alex a
parseError (Token _ s p, exps) =
  alexErrorWPos p ("parse error on input '" ++ s ++ "', "
                   ++ "expected one of: " ++ intercalate ", " exps)

parseDecls :: FilePath -> String -> Either String [KindDecl]
parseDecls = runAlexOnFile parseDeclsM

}
