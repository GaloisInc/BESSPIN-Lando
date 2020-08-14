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
  ( ppDecl
  , ppLType
  , ppType
  , ppLExpr
  , ppExpr
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

ppDecl :: Decl -> PP.Doc
ppDecl (KindDecl kd@Kind{ kindType = L _ (StructType flds) }) =
  ppLText (kindName kd) PP.<+> PP.colon
  PP.<+> PP.text "kind" PP.<+> PP.text "of" PP.<+> PP.text "struct"
  PP.$$ PP.nest 2 (ppWClause "with" (fmap ppField flds))
  PP.$$ PP.nest 2 (ppWClause "where" (ppLExpr <$> kindConstraints kd))
ppDecl (KindDecl kd) =
  ppLText (kindName kd) PP.<+> PP.colon
  PP.<+> PP.text "kind" PP.<+> PP.text "of" PP.<+> ppLType (kindType kd)
  PP.$$ PP.nest 2 (ppWClause "where" (ppLExpr <$> kindConstraints kd))
ppDecl (TypeSynDecl nm tp) =
  PP.text "type" PP.<+> ppLText nm PP.<+> PP.text "=" PP.<+> ppLType tp
ppDecl (AbsTypeDecl nm) =
  PP.text "abstract" PP.<+> PP.text "type" PP.<+> ppLText nm
ppDecl (AbsFunctionDecl nm (FunType _ argtps rettp)) =
  PP.text "abstract" PP.<+> ppLText nm PP.<+> PP.text ":"
  PP.<+> ppFunArgTypes argtps PP.<+> PP.text "->" PP.<+> ppLType rettp

ppFunArgTypes :: [LType] -> PP.Doc
ppFunArgTypes [] = PP.text "()"
ppFunArgTypes [tp] = ppLType tp
ppFunArgTypes args = PP.parens $ commas (ppLType <$> args)


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
  SetType typ ->
    PP.text "subset" PP.<+> ppLType typ
  StructType flds -> PP.text "struct" PP.<+> withClause
    where withClause = ppWClause "with" (fmap ppField flds)
  KindNames ks -> PP.hsep (fmap ppLText ks)

ppLExpr :: LExpr -> PP.Doc
ppLExpr = ppExpr . unLoc

ppLExpr' :: Bool
         -> LExpr -> PP.Doc
ppLExpr' top = ppExpr' top . unLoc

ppExpr :: Expr -> PP.Doc
ppExpr = ppExpr' True

ppExpr' :: Bool
        -> Expr -> PP.Doc
ppExpr' _ (BoolLit True) = PP.text "true"
ppExpr' _ (BoolLit False) = PP.text "false"
ppExpr' _ (IntLit x) = PP.integer x
ppExpr' _ (EnumLit e) = ppLText e
ppExpr' _ (SetLit es) =
  PP.braces (commas (ppLText <$> es))
ppExpr' _ SelfExpr = PP.text "self"
ppExpr' _ (VarExpr f) = ppLText f
ppExpr' top (FieldExpr ctxExpr f) =
  ppLExpr' top ctxExpr PP.<> PP.text "." PP.<> ppLText f
ppExpr' _ (ApplyExpr fn es) =
  ppLText fn PP.<>
  PP.parens (commas (fmap (ppLExpr' True) es))
ppExpr' False e = PP.parens (ppExpr' True e)
ppExpr' _ (StructExpr Nothing fls) =
  PP.text "struct" PP.<+> PP.text "with"
  PP.<+> commas (fmap ppFieldValue fls)
ppExpr' _ (StructExpr (Just tp) fls) =
  ppLType tp PP.<+> PP.text "with"
  PP.<+> commas (fmap ppFieldValue fls)
ppExpr' _ (EqExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.equals PP.<+> ppLExpr' False e2
ppExpr' _ (NeqExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "!=" PP.<+> ppLExpr' False e2
ppExpr' _ (LteExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "<=" PP.<+> ppLExpr' False e2
ppExpr' _ (LtExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "<" PP.<+> ppLExpr' False e2
ppExpr' _ (GteExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text ">=" PP.<+> ppLExpr' False e2
ppExpr' _ (GtExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text ">" PP.<+> ppLExpr' False e2
ppExpr' _ (PlusExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "+" PP.<+> ppLExpr' False e2
ppExpr' _ (MinusExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "-" PP.<+> ppLExpr' False e2
ppExpr' _ (TimesExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "*" PP.<+> ppLExpr' False e2
ppExpr' _ (ModExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "%" PP.<+> ppLExpr' False e2
ppExpr' _ (DivExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "/" PP.<+> ppLExpr' False e2
ppExpr' _ (AbsExpr e) =
  PP.text "abs" PP.<+> ppLExpr' False e
ppExpr' _ (NegExpr e) =
  PP.text "-" PP.<+> ppLExpr' False e
ppExpr' _ (MemberExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "in" PP.<+> ppLExpr' False e2
ppExpr' _ (NotMemberExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "notin" PP.<+> ppLExpr' False e2
ppExpr' _ (SubsetExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "subset" PP.<+> ppLExpr' False e2
ppExpr' _ (NonEmptyExpr e) =
  PP.text "nonempty" PP.<+> ppLExpr' False e
ppExpr' _ (SizeExpr e) =
  PP.text "size" PP.<+> ppLExpr' False e
ppExpr' _ (DiffExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "\\" PP.<+> ppLExpr' False e2
ppExpr' _ (AndExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "&" PP.<+> ppLExpr' False e2
ppExpr' _ (OrExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "|" PP.<+> ppLExpr' False e2
ppExpr' _ (XorExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "^" PP.<+> ppLExpr' False e2
ppExpr' _ (ImpliesExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "=>" PP.<+> ppLExpr' False e2
ppExpr' _ (IffExpr e1 e2) =
  ppLExpr' False e1 PP.<+> PP.text "<=>" PP.<+> ppLExpr' False e2
ppExpr' _ (NotExpr e) = PP.text "!" PP.<+> ppLExpr' False e
ppExpr' _ (IsInstanceExpr e t) =
  ppLExpr' False e PP.<+> PP.colon PP.<+> ppLType t

ppFieldValue :: (LText, LExpr) -> PP.Doc
ppFieldValue (nm, x) =
  ppLText nm PP.<+> PP.equals PP.<+> ppLExpr x
