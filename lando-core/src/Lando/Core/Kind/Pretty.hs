{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Lando.Core.Kind.Pretty
Description : Pretty printing functions for types defined in 'Lando.Core.Kind'.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines pretty printing functions for the core LOBOT types.
-}

module Lando.Core.Kind.Pretty
  ( ppKind
  , ppInstance
  , ppExpr
  , ppLiteral
  ) where

import Lando.Core.Kind

import qualified Text.PrettyPrint as PP
import qualified Data.Text        as T

import Data.Parameterized.List
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Prelude hiding ((!!))

symbolDoc :: SymbolRepr nm -> PP.Doc
symbolDoc = PP.text . T.unpack . symbolRepr

commas :: [PP.Doc] -> PP.Doc
commas = PP.cat . PP.punctuate (PP.text ", ")

vcommas :: [PP.Doc] -> PP.Doc
vcommas = PP.vcat . PP.punctuate (PP.text ", ")

ppKind :: Kind ktps -> PP.Doc
ppKind kd@Kind{..} = PP.text "kind" PP.<+> PP.text kindName
                     PP.$$ PP.nest 2 withClause
                     PP.$$ PP.nest 2 whereClause
  where withClause  = case kindFields of
          Nil -> PP.empty
          _ -> PP.text "with" PP.<+> vcommas (toListFC ppFieldRepr kindFields)
        whereClause = case kindConstraints of
          [] -> PP.empty
          _ -> PP.text "where" PP.<+> vcommas (ppExpr kd <$> kindConstraints)

ppFieldRepr :: FieldRepr ftp -> PP.Doc
ppFieldRepr FieldRepr{..} = symbolDoc fieldName PP.<+>
                            PP.colon PP.<+>
                            ppTypeRepr fieldType

ppTypeRepr :: TypeRepr tp -> PP.Doc
ppTypeRepr tp = case tp of
  BoolRepr -> PP.text "bool"
  IntRepr -> PP.text "int"
  EnumRepr syms ->
    PP.braces (commas (toListFC symbolDoc syms))
  SetRepr syms ->
    PP.text "subset" PP.<+>
    PP.braces (commas (toListFC symbolDoc syms))
  KindRepr flds -> PP.text "kind" PP.<+> withClause
    where withClause = case flds of
            Nil -> PP.empty
            _ -> PP.text "with" PP.<+> PP.vcat (toListFC ppFieldRepr flds)

ppLiteral :: Literal tp -> PP.Doc
ppLiteral (BoolLit True) = PP.text "true"
ppLiteral (BoolLit False) = PP.text "false"
ppLiteral (IntLit x) = PP.integer x
ppLiteral (EnumLit cs i) = symbolDoc (cs !! i)
ppLiteral (SetLit cs is) =
  PP.braces (commas (viewSome (symbolDoc . (cs !!)) <$> is))
ppLiteral (KindLit inst) = ppInstance inst

ppFieldLiteral :: FieldLiteral ftp -> PP.Doc
ppFieldLiteral FieldLiteral{..} =
  symbolDoc fieldLiteralName PP.<+> PP.equals PP.<+> ppLiteral fieldLiteralValue

ppInstance :: Instance ktps -> PP.Doc
ppInstance (Instance fls) =
  PP.text "instance" PP.<+> withClause
  where withClause = case fls of
          Nil -> PP.empty
          _ -> PP.text "with" PP.<+> commas (toListFC ppFieldLiteral fls)

exprKindFields :: Kind ktps
               -> Expr ktps (KindType ktps')
               -> List FieldRepr ktps'
exprKindFields _ (LiteralExpr (KindLit (Instance flds))) =
  fmapFC fieldLiteralType flds
exprKindFields kd SelfExpr = kindFields kd
exprKindFields kd (FieldExpr kdExpr i) =
  let KindRepr flds = fieldType (exprKindFields kd kdExpr !! i)
  in flds

ppExpr :: Kind ktps -> Expr ktps tp -> PP.Doc
ppExpr = ppExpr' True

ppExpr' :: Bool -> Kind ktps -> Expr ktps tp -> PP.Doc
ppExpr' _ _ (LiteralExpr l) = ppLiteral l
ppExpr' _ _ SelfExpr = PP.text "self"
ppExpr' _ Kind{..} (FieldExpr SelfExpr i) = symbolDoc (fieldName (kindFields !! i))
ppExpr' top kd (FieldExpr kdExpr i) =
  ppExpr' top kd kdExpr PP.<> PP.text "." PP.<>
  symbolDoc (fieldName (exprKindFields kd kdExpr !! i))
ppExpr' False kd e = PP.parens (ppExpr' True kd e)
ppExpr' _ kd (EqExpr e1 e2) =
  ppExpr' False kd e1 PP.<+> PP.equals PP.<+> ppExpr' False kd e2
ppExpr' _ kd (LteExpr e1 e2) =
  ppExpr' False kd e1 PP.<+> PP.text "<=" PP.<+> ppExpr' False kd e2
ppExpr' _ kd (MemberExpr e1 e2) =
  ppExpr' False kd e1 PP.<+> PP.text "in" PP.<+> ppExpr' False kd e2
ppExpr' _ kd (ImpliesExpr e1 e2) =
  ppExpr' False kd e1 PP.<+> PP.text "=>" PP.<+> ppExpr' False kd e2
ppExpr' _ kd (NotExpr e) = PP.text "not" PP.<+> ppExpr' False kd e
