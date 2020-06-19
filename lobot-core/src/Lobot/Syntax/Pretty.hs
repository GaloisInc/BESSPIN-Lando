{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Lobot.Syntax.Pretty
Description : Pretty printing functions for types defined in 'Lobot.Syntax'.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines pretty printing functions for the core LOBOT untyped syntax.
-}

module Lobot.Syntax.Pretty
  ( ppKindDecl
  , ppLType
  , ppType
  , ppLExpr
  , ppExpr
  , ppLLiteral
  , ppLiteral
  , ppText
  , ppLText
  ) where

import Lobot.Syntax

import qualified Text.PrettyPrint as PP
import qualified Data.Text        as T

ppLText :: LText -> PP.Doc
ppLText = ppText . unLoc

ppText :: T.Text -> PP.Doc
ppText = PP.text . T.unpack

commas :: [PP.Doc] -> PP.Doc
commas = PP.cat . PP.punctuate (PP.text ", ")

vcommas :: [PP.Doc] -> PP.Doc
vcommas = PP.vcat . PP.punctuate (PP.text ", ")

ppKindDecl :: KindDecl -> PP.Doc
ppKindDecl kd@KindDecl{ kindDeclType = L _ (StructType flds) } =
  ppText (kindDeclName kd) PP.<+> PP.colon
  PP.<+> PP.text "kind" PP.<+> PP.text "of" PP.<+> PP.text "struct"
  PP.$$ PP.nest 2 (ppWClause "with" (fmap ppField flds))
  PP.$$ PP.nest 2 (ppWClause "where" (ppLExpr <$> kindDeclConstraints kd))
ppKindDecl kd =
  ppText (kindDeclName kd) PP.<+> PP.colon
  PP.<+> PP.text "kind" PP.<+> PP.text "of" PP.<+> ppLType (kindDeclType kd)
  PP.$$ PP.nest 2 (ppWClause "where" (ppLExpr <$> kindDeclConstraints kd))

ppWClause :: String -> [PP.Doc] -> PP.Doc
ppWClause _ [] = PP.empty
ppWClause w xs = PP.text w PP.<+> vcommas xs

ppField :: (LText, LType) -> PP.Doc
ppField (fieldName, fieldType) = ppLText fieldName PP.<+>
                                 PP.colon PP.<+>
                                 ppLType fieldType

ppLType :: LType -> PP.Doc
ppLType = ppType . unLoc

ppType :: Type -> PP.Doc
ppType tp = case tp of
  BoolType -> PP.text "bool"
  IntType -> PP.text "int"
  EnumType syms ->
    PP.braces (commas (fmap ppText syms))
  SetType syms ->
    PP.text "subset" PP.<+>
    PP.braces (commas (fmap ppText syms))
  StructType flds -> PP.text "struct" PP.<+> withClause
    where withClause = ppWClause "with" (fmap ppField flds)
  KindNames ks -> PP.hsep (fmap ppLText ks)

ppLLiteral :: LLiteral -> PP.Doc
ppLLiteral = ppLiteral . unLoc

ppLiteral :: Literal -> PP.Doc
ppLiteral (BoolLit True) = PP.text "true"
ppLiteral (BoolLit False) = PP.text "false"
ppLiteral (IntLit x) = PP.integer x
ppLiteral (EnumLit e) = ppLText e
ppLiteral (SetLit es) =
  PP.braces (commas (ppLText <$> es))
ppLiteral (StructLit fls) = PP.text "struct" PP.<+> withClause
  where withClause = case fls of
          [] -> PP.empty
          _ -> PP.text "with" PP.<+> commas (fmap ppFieldLiteral fls)

ppFieldLiteral :: (LText, LLiteral) -> PP.Doc
ppFieldLiteral (fieldLiteralName, fieldLiteralValue) =
  ppLText fieldLiteralName PP.<+> PP.equals PP.<+> ppLLiteral fieldLiteralValue

ppLExpr :: LExpr -> PP.Doc
ppLExpr = ppExpr . unLoc

ppLExpr' :: Bool
         -> LExpr -> PP.Doc
ppLExpr' top = ppExpr' top . unLoc

ppExpr :: Expr -> PP.Doc
ppExpr = ppExpr' True

ppExpr' :: Bool
        -> Expr -> PP.Doc
ppExpr' _ (LiteralExpr l) = ppLLiteral l
ppExpr' _ SelfExpr = PP.text "self"
ppExpr' _ (SelfFieldExpr f) = ppLText f
ppExpr' top (FieldExpr ctxExpr f) =
  ppLExpr' top ctxExpr PP.<> PP.text "." PP.<> ppLText f
ppExpr' _ (ApplyExpr fn es) =
  ppLText fn PP.<>
  PP.parens (commas (fmap (ppLExpr' True) es))
ppExpr' False e = PP.parens (ppExpr' True e)
ppExpr' _ (EqExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.equals PP.<+> ppLExpr' False e2
ppExpr' _ (LteExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "<=" PP.<+> ppLExpr' False e2
ppExpr' _ (PlusExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "+" PP.<+> ppLExpr' False e2
ppExpr' _ (MemberExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "in" PP.<+> ppLExpr' False e2
ppExpr' _ (ImpliesExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "=>" PP.<+> ppLExpr' False e2
ppExpr' _ (NotExpr e) = PP.text "not" PP.<+> ppLExpr' False e
ppExpr' _ (IsInstanceExpr e t) =
  ppLExpr' False e PP.<+> PP.colon PP.<+> ppLType t
