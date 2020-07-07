{
{-|
Module      : Lobot.Parser
Description : A parser for the untyped AST of the Lobot sublanguage.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module provides a parser for the Lobot sublanguage.
-}
module Lobot.Parser
  ( parseDecls )
  where

import Data.Text (Text, pack)
import Data.List (concatMap, intercalate)

import Lobot.Syntax
import Lobot.Lexer
import Lobot.Utils
}

%expect 0

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
  'check'     { L _ (Token CHECK _) }
  'on'        { L _ (Token ON _) }
  'that'      { L _ (Token THAT _) }
  'type'      { L _ (Token TYPE _) }
  'abstract'  { L _ (Token ABSTRACT _) }
  '->'        { L _ (Token ARROW _) }
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
  -- layout tokens are handled differently
  LAYEND_WHERE  { L _ (Token (LAYEND (FromOther WHERE)) _) }
  LAYEND_THAT   { L _ (Token (LAYEND (FromOther THAT)) _) }
  LAYEND_RBRACE { L _ (Token (LAYEND (FromOther RBRACE)) _) }
  LAYEND_OTHER  { L _ (Token (LAYEND (FromOther _)) _) }
  LAYEND_NL     { L _ (Token (LAYEND _) _) }
  LAYSEP        { L _ (Token LAYSEP _) }

%nonassoc ':'
%left     '=>'
%nonassoc '=' '<='
%left     '+'
%nonassoc 'not'
%nonassoc 'in'
%left     '.'

%%

decls :: { [Decl] }
decls : {- empty -}           { [] }
      | decl nlLAYEND decls   { $1 : $3 }

decl :: { Decl }
decl : ident ':' kindDeclType         { KindDecl $ Kind (locText $1) (fst $3) (snd $3) }
     | ident ':' checkDeclType        { CheckDecl $ Check (locText $1) (fst3 $3) (snd3 $3) (thd3 $3) }
     | 'type' ident '=' type          { TypeSynDecl (locText $2) $4 }
     | 'abstract' 'type' ident        { AbsTypeDecl (locText $3) }
     | 'abstract' ident ':' funType   { AbsFunctionDecl (locText $2) ($4 (locText $2)) }


kindDeclType :: { (LType,[LExpr]) }
kindDeclType : 'kind' 'of' kindType nlLAYEND                                     { ($3,[]) }
             | 'kind' 'of' kindType whereLAYEND 'where' optLAYSEP cns nlLAYEND   { ($3,$7) }

kindType :: { LType }
kindType : type                                         { $1 }
         | 'struct' nlLAYEND 'with' optLAYSEP fields    { loc $1 $ StructType $5 }
         -- ^ Note: We don't need a LAYEND after the 'with' here as it's
         --   handled by the LAYENDs in `kindDeclType`.

checkDeclType :: { ([(LText,LType)],[LExpr],[LExpr]) }
checkDeclType : 'check' nlLAYEND 'on' optLAYSEP fields thatLAYEND 'that' optLAYSEP cns nlLAYEND { ($5, [], $9) }
              | 'check' nlLAYEND 'on' optLAYSEP fields whereLAYEND 'where' optLAYSEP cns thatLAYEND 'that' optLAYSEP cns nlLAYEND { ($5, $9, $13) }

cns : expr              { [$1] }
    | expr anySep       { [$1] }
    | expr anySep cns   { $1 : $3 }


funType :: { LText -> FunctionType }
funType : type '->' type nlLAYEND               { \nm -> FunType nm [$1] $3 }
        | '(' ')' '->' type nlLAYEND            { \nm -> FunType nm [] $4 }
        | '(' argTypes ')' '->' type nlLAYEND   { \nm -> FunType nm $2 $5 }

argTypes : type                     { [$1] }
         | type commaSep argTypes   { $1 : $3 }


type    : 'bool'                                       { loc $1 $ BoolType }
        | 'int'                                        { loc $1 $ IntType }
        | '{' enumIdents '}'                           { loc $1 $ EnumType (fmap unLoc $2) }
        | 'subset' '{' enumIdents '}'                  { loc $1 $ SetType (fmap unLoc $3) }
        | 'struct' 'with' optLAYSEP fields anyLAYEND   { loc $1 $ StructType $4 }
        | 'struct' 'with' optLAYSEP '{' '}'            { loc $1 $ StructType [] }
        | 'struct' 'with' optLAYSEP '{' fields '}'     { loc $1 $ StructType $5 }
        | kindNames                                    { $1 }

fields :: { [(LText, LType)] }
fields : field                   { [$1] }
       | field anySep          { [$1] }
       | field anySep fields   { $1 : $3 }

field : ident ':' type anyLAYEND { (locText $1, $3) }

kindNames : idents { loc (head $1) $ KindNames $1 }


expr :: { LExpr }
expr : lit                         { loc $1 $ LiteralExpr $1 }
     | 'self'                      { loc $1 $ SelfExpr }
     | ident                       { loc $1 $ VarExpr (locText $1) }
     | expr '.' ident              { loc $1 $ FieldExpr $1 (locText $3) }
     | expr '=' expr               { loc $1 $ EqExpr $1 $3 }
     | expr '<=' expr              { loc $1 $ LteExpr $1 $3 }
     | expr '+' expr               { loc $1 $ PlusExpr $1 $3 }
     | expr 'in' expr              { loc $1 $ MemberExpr $1 $3 }
     | expr '=>' expr              { loc $1 $ ImpliesExpr $1 $3 }
     | 'not' expr                  { loc $1 $ NotExpr $2 }
     | ident '(' ')'               { loc $1 $ ApplyExpr (locText $1) [] }
     | ident '(' args ')'          { loc $1 $ ApplyExpr (locText $1) $3 }
     | expr ':' idents anyLAYEND   { loc $1 $ IsInstanceExpr $1 (loc (head $3) $ KindNames $3) }
     | '(' expr ')'                { loc $1 $ unLoc $2 }

args : expr                 { [$1] }
     | expr commaSep args   { $1 : $3 }


lit :: { LLiteral }
lit : 'true'                              { loc $1 $ BoolLit True }
    | 'false'                             { loc $1 $ BoolLit False }
    | int                                 { loc $1 $ IntLit (tkInt $1) }
    | enumIdent                           { loc $1 $ EnumLit (locText $1) }
    | '{' enumIdents '}'                  { loc $1 $ SetLit $2 }
    | 'struct' 'with' '{' fieldvals '}'   { loc $1 $ StructLit Nothing $4 }
    | kindNames 'with' '{' fieldvals '}'  { loc $1 $ StructLit (Just $1) $4 }

fieldvals :: { [(LText, LLiteral)] }
fieldvals : {- empty -}                   { [] }
          | fieldval                      { [$1] }
          | fieldval commaSep fieldvals   { $1 : $3 }

fieldval : ident '=' lit                  { (locText $1, $3) }


enumIdents :: { [LText] }
enumIdents : enumIdent commaSep enumIdents   { locText $1 : $3 }
           | enumIdent                       { locText $1 : [] }
           | {- empty -}                     { [] }

idents :: { [LText] }
idents : ident          { locText $1 : [] }
       | ident idents   { locText $1 : $2 }


nlLAYEND    : LAYEND_NL {}
whereLAYEND : LAYEND_NL {} | LAYEND_WHERE {}
thatLAYEND  : LAYEND_NL {} | LAYEND_THAT {}
anyLAYEND   : LAYEND_NL {} | LAYEND_WHERE {} | LAYEND_THAT {} | LAYEND_RBRACE {} | LAYEND_OTHER {}

optLAYSEP : {- empty -} {} | LAYSEP {}

anySep : ','            {}
       | LAYSEP         {}
       | ',' LAYSEP     {}

commaSep : ','          {}
         | LAYSEP ','   {}

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
parseError (L p (Token (LAYEND FromNewline) _), es) =
  alexErrorWPos p ("expected more indentation")
parseError (L p (Token (LAYEND (FromOther WHERE)) _), es) =
  alexErrorWPos p ("parse error on input 'where' (layout)" ++ fmtExpected es)
parseError (L p (Token (LAYEND (FromOther THAT)) _), es) =
  alexErrorWPos p ("parse error on input 'that' (layout)" ++ fmtExpected es)
parseError (L p (Token (LAYEND (FromOther RBRACE)) _), es) =
  alexErrorWPos p ("parse error on input '}' (layout)" ++ fmtExpected es)
parseError (L p (Token (LAYEND (FromOther tk)) _), es) =
  alexErrorWPos p ("(layout) parse error on token " ++ show tk ++ " (layout)" ++ fmtExpected es)
parseError (L p (Token LAYSEP _), es) =
  alexErrorWPos p ("unexpected newline (layout)" ++ fmtExpected es)
parseError (L p (Token (LAYEND FromEOF) s), es) =
  alexErrorWPos p ("unexpected end of file (layout)" ++ fmtExpected es)
parseError (L p (Token EOF s), es) =
  alexErrorWPos p ("unexpected end of file" ++ fmtExpected es)
parseError (L p (Token _ s), es) =
  alexErrorWPos p ("parse error on input '" ++ s ++ "'" ++ fmtExpected es)

fmtExpected :: [String] -> String
fmtExpected = go . concatMap modifyStr
  where modifyStr :: String -> [String]
        modifyStr "LAYEND_WHERE"  = ["'where' (layout)"]
        modifyStr "LAYEND_THAT"   = ["'that' (layout)"]
        modifyStr "LAYEND_RBRACE" = []
        modifyStr "LAYEND_OTHER"  = []
        modifyStr "LAYEND_NL"     = ["different indentation"]
        modifyStr "LAYSEP"        = []
        modifyStr "EOF"           = []
        modifyStr s               = [s]
        go :: [String] -> String
        go []       = ""
        go [e]      = ", expected " ++ e
        go [e1,e2]  = ", expected " ++ e1 ++ " or " ++ e2
        go exps     = ", expected one of: " ++ intercalate ", " exps

parseDecls :: FilePath -> String -> Either String [Decl]
parseDecls = runAlexOnFile parseDeclsM

}
