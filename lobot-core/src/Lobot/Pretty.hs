{-# LANGUAGE GADTs #-}
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
  , ppLiteral
  ) where

import Lobot.Kind
import Lobot.Types

import qualified Text.PrettyPrint as PP
import qualified Data.Text        as T

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
  PP.$$ PP.nest 2 (ppWClause "where" (ppExpr (kindFunctionEnv kd) (kindType kd) <$> kindConstraints kd))
ppKind kd =
  PP.text (T.unpack $ kindName kd) PP.<+> PP.colon
  PP.<+> PP.text "kind" PP.<+> PP.text "of" PP.<+> ppTypeRepr (kindType kd)
  PP.$$ PP.nest 2 (ppWClause "where" (ppExpr (kindFunctionEnv kd) (kindType kd) <$> kindConstraints kd))

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

ppLiteral :: Literal tp -> PP.Doc
ppLiteral (BoolLit True) = PP.text "true"
ppLiteral (BoolLit False) = PP.text "false"
ppLiteral (IntLit x) = PP.integer x
ppLiteral (EnumLit cs i) = symbolDoc (cs ! i)
ppLiteral (SetLit cs is) =
  PP.braces (commas (viewSome (symbolDoc . (cs !)) <$> is))
ppLiteral (StructLit fls) = PP.text "struct" PP.<+> withClause
  where withClause = case fls of
          Empty -> PP.empty
          _ -> PP.text "with" PP.<+> commas (toListFC ppFieldLiteral fls)

ppFieldLiteral :: FieldLiteral ftp -> PP.Doc
ppFieldLiteral FieldLiteral{..} =
  symbolDoc fieldLiteralName PP.<+> PP.equals PP.<+> ppLiteral fieldLiteralValue

exprStructFields :: Assignment FunctionTypeRepr env
                 -> TypeRepr ctx
                 -> Expr env ctx (StructType ftps)
                 -> Assignment FieldRepr ftps
exprStructFields _ _ (LiteralExpr (StructLit fls)) = fmapFC fieldLiteralType fls
exprStructFields _ (StructRepr fls) SelfExpr = fls
exprStructFields env tp (FieldExpr structExpr i) =
  let StructRepr fls = fieldType (exprStructFields env tp structExpr ! i)
  in fls
exprStructFields env _ (ApplyExpr fi _) =
  let FunctionTypeRepr _ _ (StructRepr fls) = env ! fi
  in fls

ppExpr :: Assignment FunctionTypeRepr env -> TypeRepr ctx -> Expr env ctx tp -> PP.Doc
ppExpr = ppExpr' True

ppExpr' :: Bool
        -> Assignment FunctionTypeRepr env
        -> TypeRepr ctx
        -> Expr env ctx tp -> PP.Doc
ppExpr' _ _ _ (LiteralExpr l) = ppLiteral l
ppExpr' _ _ _ SelfExpr = PP.text "self"
ppExpr' _ _ (StructRepr flds) (FieldExpr SelfExpr i) =
  symbolDoc (fieldName (flds ! i))
ppExpr' top env ctx (FieldExpr ctxExpr i) =
  ppExpr' top env ctx ctxExpr PP.<> PP.text "." PP.<>
  symbolDoc (fieldName (exprStructFields env ctx ctxExpr ! i))
ppExpr' _ env ctx (ApplyExpr fi es) =
  let FunctionTypeRepr fnm _ _ = env ! fi
  in symbolDoc fnm PP.<>
     PP.parens (commas (toListFC (ppExpr' True env ctx) es))
ppExpr' False env ctx e = PP.parens (ppExpr' True env ctx e)
ppExpr' _ env ctx (EqExpr e1 e2) =
  ppExpr' False env ctx e1 PP.<+> PP.equals PP.<+> ppExpr' False env ctx e2
ppExpr' _ env ctx (LteExpr e1 e2) =
  ppExpr' False env ctx e1 PP.<+> PP.text "<=" PP.<+> ppExpr' False env ctx e2
ppExpr' _ env ctx (PlusExpr e1 e2) =
  ppExpr' False env ctx e1 PP.<+> PP.text "+" PP.<+> ppExpr' False env ctx e2
ppExpr' _ env ctx (MemberExpr e1 e2) =
  ppExpr' False env ctx e1 PP.<+> PP.text "in" PP.<+> ppExpr' False env ctx e2
ppExpr' _ env ctx (ImpliesExpr e1 e2) =
  ppExpr' False env ctx e1 PP.<+> PP.text "=>" PP.<+> ppExpr' False env ctx e2
ppExpr' _ env ctx (NotExpr e) = PP.text "not" PP.<+> ppExpr' False env ctx e
