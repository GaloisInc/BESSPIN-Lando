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

%tokentype { LToken }
%errorhandlertype explist
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexer } { L _ (Token EOF _) }

%token
  -- This token list must match that in Lexer.y!
  -- This list is what's used for pretty-printing expected tokens :(
  'bool'      { L _ (Token BOOL _) }
  'int'       { L _ (Token INTTYPE _) }
  'subset'    { L _ (Token SET _) }
  'struct'    { L _ (Token STRUCT _) }
  'with'      { L _ (Token WITH _) }
  ':'         { L _ (Token COLON _) }
  ','         { L _ (Token COMMA _) }
  'kind'      { L _ (Token KIND _) }
  'of'        { L _ (Token OF _) }
  'where'     { L _ (Token WHERE _) }
  'self'      { L _ (Token SELF _) }
  '.'         { L _ (Token DOT _) }
  '='         { L _ (Token EQUALS _) }
  '<='        { L _ (Token LTE _) }
  '+'         { L _ (Token PLUS _) }
  'in'        { L _ (Token IN _) }
  '=>'        { L _ (Token IMPLIES _) }
  'not'       { L _ (Token NOT _) }
  'true'      { L _ (Token TRUE _) }
  'false'     { L _ (Token FALSE _) }
  '{'         { L _ (Token LBRACE _) }
  '}'         { L _ (Token RBRACE _) }
  '('         { L _ (Token LPAREN _) }
  ')'         { L _ (Token RPAREN _) }
  int         { L _ (Token (INT _) _) }
  ident       { L _ (Token (IDLC _) _) }
  enumIdent   { L _ (Token (IDUC _) _) }

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
decl : ident ':' 'kind' 'of' topType                 { KindDecl (tkText $1) $5 [] }
     | ident ':' 'kind' 'of' topType 'where' exprs   { KindDecl (tkText $1) $5 $7 }


topType :: { LType }
topType : type                             { $1 }
        | 'struct' 'with' fields           { loc $1 $ StructType $3 }

type    : 'bool'                           { loc $1 $ BoolType }
        | 'int'                            { loc $1 $ IntType }
        | '{' enumIdents '}'               { loc $1 $ EnumType (fmap unLoc $2) }
        | 'subset' '{' enumIdents '}'      { loc $1 $ SetType (fmap unLoc $3) }
        | 'struct'                         { loc $1 $ StructType [] }
        | 'struct' 'with' '{' fields '}'   { loc $1 $ StructType $4 }
        | idents                           { loc (head $1) $ KindNames $1 }

fields :: { [(LText, LType)] }
fields : field                 { [$1] }
       | field ',' fields      { $1 : $3 }

field : ident ':' type         { (locText $1, $3) }


expr :: { LExpr }
expr : lit                   { loc $1 $ LiteralExpr $1 }
     | 'self'                { loc $1 $ SelfExpr }
     | ident                 { loc $1 $ SelfFieldExpr (locText $1) }
     | expr '.' ident        { loc $1 $ FieldExpr $1 (locText $3) }
     | expr '=' expr         { loc $1 $ EqExpr $1 $3 }
     | expr '<=' expr        { loc $1 $ LteExpr $1 $3 }
     | expr '+' expr         { loc $1 $ PlusExpr $1 $3 }
     | expr 'in' expr        { loc $1 $ MemberExpr $1 $3 }
     | expr '=>' expr        { loc $1 $ ImpliesExpr $1 $3 }
     | 'not' expr            { loc $1 $ NotExpr $2 }
     | ident '(' exprs0 ')'  { loc $1 $ ApplyExpr (locText $1) $3 }
     | expr ':' idents       { loc $1 $ IsInstanceExpr $1 (loc (head $3) $ KindNames $3) }
     | '(' expr ')'          { loc $1 $ unLoc $2 }

exprs0 : {- empty -} { [] }
       | exprs       { $1 }

exprs : expr             { [$1] }
      | expr ',' exprs   { $1 : $3 }


lit :: { LLiteral }
lit : 'true'                              { loc $1 $ BoolLit True }
    | 'false'                             { loc $1 $ BoolLit False }
    | int                                 { loc $1 $ IntLit (tkInt $1) }
    | enumIdent                           { loc $1 $ EnumLit (locText $1) }
    | '{' enumIdents '}'                  { loc $1 $ SetLit $2 }
    | 'struct' 'with' '{' fieldvals '}'   { loc $1 $ StructLit $4 }

fieldvals :: { [(LText, LLiteral)] }
fieldvals : {- empty -}              { [] }
          | fieldval ',' fieldvals   { $1 : $3 }

fieldval : ident '=' lit             { (locText $1, $3) }


enumIdents :: { [LText] }
enumIdents : {- empty -}    { [] }
           | enumIdent ',' enumIdents   { locText $1 : $3 }
           | enumIdent                  { locText $1 : [] }

idents :: { [LText] }
idents : ident          { locText $1 : [] }
       | ident idents   { locText $1 : $2 }


{

locText :: LToken -> LText
locText (L p (Token (IDLC s) _)) = L p (pack s)
locText (L p (Token (IDUC s) _)) = L p (pack s)
locText (L p (Token _        s)) = L p (pack s)

tkText :: LToken -> Text
tkText = unLoc . locText

tkInt :: LToken -> Integer
tkInt (L _ (Token (INT z) _)) = z
tkInt (L _ (Token _ s)) = read s

parseError :: (LToken, [String]) -> Alex a
parseError (L p (Token _ s), []) =
  alexErrorWPos p ("parse error on input '" ++ s ++ "'")
parseError (L p (Token _ s), [e]) =
  alexErrorWPos p ("parse error on input '" ++ s ++ "', expected " ++ e)
parseError (L p (Token _ s), exps) =
  alexErrorWPos p ("parse error on input '" ++ s ++ "', "
                   ++ "expected one of: " ++ intercalate ", " exps)

parseDecls :: FilePath -> String -> Either String [KindDecl]
parseDecls = runAlexOnFile parseDeclsM

}
