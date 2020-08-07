{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Lobot.Pretty
Description : Pretty printing functions for types defined in 'Lobot.Kind'.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines pretty printing functions for the core LOBOT types.
-}

module Lobot.Pretty
  ( ppKind
  , ppFieldRepr
  , ppTypeRepr
  , ppExpr
  , ppKindExpr
  , ppFunctionCallResult
  , ppLiteral
  , ppLiteralWithKindName
  ) where

import Lobot.Expr
import Lobot.Kind
import Lobot.Types

import qualified Text.PrettyPrint as PP
import qualified Data.Text        as T

import Data.Functor.Const
import Data.Parameterized.Context
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

ppKind :: Kind env tp -> PP.Doc
ppKind kd@Kind{ kindType = StructRepr flds } =
  PP.text (T.unpack $ kindName kd) PP.<+> PP.colon
  PP.<+> PP.text "kind" PP.<+> PP.text "of" PP.<+> PP.text "struct"
  PP.$$ PP.nest 2 (ppWClause "with" (toListFC ppFieldRepr flds))
  PP.$$ PP.nest 2 (ppWClause "where" (ppKindExpr (kindFunctionEnv kd) (kindType kd) <$> kindConstraints kd))
ppKind kd =
  PP.text (T.unpack $ kindName kd) PP.<+> PP.colon
  PP.<+> PP.text "kind" PP.<+> PP.text "of" PP.<+> ppTypeRepr (kindType kd)
  PP.$$ PP.nest 2 (ppWClause "where" (ppKindExpr (kindFunctionEnv kd) (kindType kd) <$> kindConstraints kd))

ppWClause :: String -> [PP.Doc] -> PP.Doc
ppWClause _ [] = PP.empty
ppWClause w xs = PP.text w PP.<+> vcommas xs

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
  StructRepr flds -> PP.text "struct" PP.<+> withClause
    where withClause = ppWClause "with" (toListFC ppFieldRepr flds)
  AbsRepr s -> PP.text (T.unpack (symbolRepr s))

ppLiteral :: Literal tp -> PP.Doc
ppLiteral (BoolLit True) = PP.text "true"
ppLiteral (BoolLit False) = PP.text "false"
ppLiteral (IntLit x) = PP.integer x
ppLiteral (EnumLit cs i) = symbolDoc (cs ! i)
ppLiteral (SetLit cs is) =
  PP.braces (commas (viewSome (symbolDoc . (cs !)) <$> is))
ppLiteral (StructLit fls) =
  PP.text "struct" PP.<+> PP.text "with"
  PP.<+> PP.braces (commas (toListFC ppFieldLiteral fls))
ppLiteral (AbsLit s _) = PP.text "<" PP.<> PP.text (T.unpack (symbolRepr s)) PP.<> PP.text ">"

ppLiteralWithKindName :: T.Text -> Literal tp -> PP.Doc
ppLiteralWithKindName knm (StructLit fls) =
  PP.text (T.unpack knm) PP.<+> PP.text "with"
  PP.<+> PP.braces (commas (toListFC ppFieldLiteral fls))
ppLiteralWithKindName _ l = ppLiteral l

ppFieldLiteral :: FieldLiteral ftp -> PP.Doc
ppFieldLiteral (FieldLiteral nm _ lt) =
  symbolDoc nm PP.<+> PP.equals PP.<+> ppLiteral lt

exprStructFields :: Assignment FunctionTypeRepr env
                 -> Assignment TypeRepr ctx
                 -> Expr env ctx (StructType ftps)
                 -> Assignment FieldRepr ftps
exprStructFields _ _ (LiteralExpr (StructLit fls)) =
  fmapFC fieldLiteralFieldType fls
exprStructFields _ ctx (VarExpr i)
  | StructRepr fls <- ctx ! i = fls
exprStructFields env ctx (FieldExpr structExpr i) =
  let StructRepr fls = fieldType (exprStructFields env ctx structExpr ! i)
  in fls
exprStructFields env _ (ApplyExpr fi _) =
  let FunctionTypeRepr _ _ (StructRepr fls) = env ! fi
  in fls

ppKindExpr :: Assignment FunctionTypeRepr env
           -> TypeRepr ktp
           -> KindExpr env ktp tp
           -> PP.Doc
ppKindExpr env ktp = ppExpr env (Empty :> ktp) (Empty :> Const "self")

ppFunctionCallResult :: Assignment FunctionTypeRepr env
                     -> Assignment TypeRepr ctx
                     -> Assignment (Const T.Text) ctx
                     -> FunctionCallResult env ctx
                     -> PP.Doc
ppFunctionCallResult env ctx nms (FunctionCallResult fi args ret _) =
  ppExpr env ctx nms (EqExpr (ApplyExpr fi args) (LiteralExpr ret)) 

ppExpr :: Assignment FunctionTypeRepr env
       -> Assignment TypeRepr ctx
       -> Assignment (Const T.Text) ctx
       -> Expr env ctx tp
       -> PP.Doc
ppExpr = ppExpr' True

ppExpr' :: Bool
        -> Assignment FunctionTypeRepr env
        -> Assignment TypeRepr ctx
        -> Assignment (Const T.Text) ctx
        -> Expr env ctx tp
        -> PP.Doc
ppExpr' _ _ _ _ (LiteralExpr l) = ppLiteral l
ppExpr' _ _ _ nms (VarExpr i) = PP.text (T.unpack . getConst $ nms ! i)
ppExpr' _ _ (Empty :> StructRepr flds) _ (FieldExpr SelfExpr i) =
  symbolDoc (fieldName (flds ! i))
ppExpr' top env ctx nms (FieldExpr ctxExpr i) =
  ppExpr' top env ctx nms ctxExpr PP.<> PP.text "." PP.<>
  symbolDoc (fieldName (exprStructFields env ctx ctxExpr ! i))
ppExpr' _ env ctx nms (ApplyExpr fi es) =
  let FunctionTypeRepr fnm _ _ = env ! fi
  in symbolDoc fnm PP.<>
     PP.parens (commas (toListFC (ppExpr' True env ctx nms) es))
ppExpr' False env ctx nms e = PP.parens (ppExpr' True env ctx nms e)
ppExpr' _ env ctx nms (EqExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.equals PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (NeqExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "!=" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (LteExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "<=" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (LtExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "<" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (GteExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text ">=" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (GtExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text ">" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (PlusExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "+" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (MinusExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "-" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (TimesExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "*" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (ModExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "%" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (DivExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "/" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (NegExpr e) =
  PP.text "-" PP.<+> ppExpr' False env ctx nms e
ppExpr' _ env ctx nms (MemberExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "in" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (NotMemberExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "notin" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (AndExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "&" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (OrExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "|" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (XorExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "^" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (ImpliesExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "=>" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (IffExpr e1 e2) =
  ppExpr' False env ctx nms e1 PP.<+> PP.text "<=>" PP.<+> ppExpr' False env ctx nms e2
ppExpr' _ env ctx nms (NotExpr e) = PP.text "not" PP.<+> ppExpr' False env ctx nms e
