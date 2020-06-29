{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module      : Lobot.TypeCheck.Monad
Description : The monad used during LOBOT type checking.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module defines the monad used while type checking the Lobot AST.
-}

module Lobot.TypeCheck.Monad where

import qualified Data.HashMap as H
import qualified Data.HashSet as HS
import qualified Text.PrettyPrint as PP

import Data.Text (Text)
import Control.Monad (foldM_)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Except (throwError)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Some
import Data.Parameterized.Context
import Prelude hiding (zipWith, unzip)

import Lobot.Lexer (AlexPosn, errorPrefix)
import Lobot.Syntax as S
import Lobot.Types as T
import Lobot.Pretty as P
import Lobot.Syntax.Pretty as S


type WithWarnings a = WriterT [TypeWarning] a

type CtxM t err = StateT (H.Map Text t) (WithWarnings (Either err))

emitWarning :: TypeWarning -> CtxM t err ()
emitWarning = tell . (:[])

ensureUnique :: (a -> Text) -> [a] -> (a -> err) -> CtxM t err ()
ensureUnique f xs err =
  foldM_ (\xset x -> if (f x) `HS.notMember` xset
                     then pure $ HS.insert (f x) xset
                     else throwError (err x)) HS.empty xs


-- | All type errors that can be generated by the typechecker.
data TypeError = TypeMismatchError S.LExpr SomeTypeOrString (Maybe SomeTypeOrString)
                 -- ^ argument order: expr, expected type, actual type
               | AbstractEqualityError S.LExpr SomeTypeOrString
               | TypeUnificationError S.LExpr SomeTypeOrString S.LExpr SomeTypeOrString
               | EnumSetUnificationError S.LExpr SomeTypeOrString S.LExpr SomeTypeOrString
               | TypeInferenceError S.LExpr
               | DuplicateEnumNameError S.LType Text
               | EmptyEnumOrSetError S.LType
               | KindUnionMismatchError LText (Some T.TypeRepr) (Some T.TypeRepr)
               | NoSuchFieldError LText S.LExpr (Some T.TypeRepr)
               | StructLiteralTypeError S.LType
               | StructLiteralNameMismatchError LText Text
               | StructLiteralLengthError AlexPosn (Some (Assignment FieldRepr)) [LText]
               | StructLiteralAbstractTypeError AlexPosn (Some (Assignment FieldRepr))
                 -- ^ argument order: kind name, expected type, actual type
               | KindNameNotInScope LText
               | FunctionNameNotInScope LText
               | FieldNameNotInScope LText
               | OtherNameNotInScope LText
               | KindNameAlreadyDefined LText
               | FunctionNameAlreadyDefined LText
               | FieldNameAlreadyDefined LText
               | FunctionArgLengthError LText (Some FunctionTypeRepr) [S.LExpr]
               | UnexpectedSelfError AlexPosn
               | InternalError AlexPosn Text
               deriving Show

-- | All warnings that can be generated by the typechecker.
data TypeWarning = EnumNameNotInScope LText

ppTypeError :: FilePath -> TypeError -> PP.Doc
ppTypeError fp (TypeMismatchError (L p x) exp_tp Nothing)
  | SomeType tp <- exp_tp, TrueRepr <- isAbstractType tp
  = PP.text (errorPrefix fp p)
    PP.<+> PP.text "Cannot construct a term of abstract type:"
    PP.<+> PP.nest 6 (ppSomeTypeOrString exp_tp)
    PP.$$  PP.nest 2 (PP.text "Type mismatch on expression:" PP.<+> S.ppExpr x)
  | otherwise
  = PP.text (errorPrefix fp p)
    PP.<+> PP.text "Type mismatch on expression:" PP.<+> S.ppExpr x
    PP.$$ PP.nest 2 (PP.text "Expected type:") PP.<+> PP.nest 6 (ppSomeTypeOrString exp_tp)
ppTypeError fp (TypeMismatchError x exp_tp (Just act_tp)) =
  ppTypeError fp (TypeMismatchError x exp_tp Nothing)
  PP.$$ PP.nest 2 (PP.text "  Actual type:") PP.<+> PP.nest 6 (ppSomeTypeOrString act_tp)
ppTypeError fp (AbstractEqualityError (L p x) tp) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Equality comparison on abstract type:" PP.<+> ppSomeTypeOrString tp
  PP.$$ PP.nest 2 (PP.text "In expression:" PP.<+> S.ppExpr x)
ppTypeError fp (TypeUnificationError (L p x) xtp (L _ y) ytp) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Could not unify the types of the following expressions:"
  PP.$$  PP.nest 2 (PP.text "-" PP.<+> S.ppExpr x)
  PP.<+> PP.nest 6 (PP.text ":" PP.<+> ppSomeTypeOrString xtp)
  PP.$$  PP.nest 2 (PP.text "-" PP.<+> S.ppExpr y)
  PP.<+> PP.nest 6 (PP.text ":" PP.<+> ppSomeTypeOrString ytp)
ppTypeError fp (EnumSetUnificationError (L p x) xtp (L _ y) ytp) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Could not unify the name sets in the types of the following expressions:"
  PP.$$  PP.nest 2 (PP.text "-" PP.<+> S.ppExpr x)
  PP.<+> PP.nest 6 (PP.text ":" PP.<+> ppSomeTypeOrString xtp)
  PP.$$  PP.nest 2 (PP.text "-" PP.<+> S.ppExpr y)
  PP.<+> PP.nest 6 (PP.text ":" PP.<+> ppSomeTypeOrString ytp)
ppTypeError fp (TypeInferenceError (L p x)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Could not infer the type of expression:" PP.<+> S.ppExpr x
ppTypeError fp (DuplicateEnumNameError (L p tp) nm) =
  PP.text (errorPrefix fp p)
  PP.<+> ppQText nm PP.<+> PP.text "appears more than once in the enum or set:" PP.<+> S.ppType tp
ppTypeError fp (EmptyEnumOrSetError (L p tp)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Empty enum or set:" PP.<+> S.ppType tp
ppTypeError fp (KindUnionMismatchError (L p k) (Some exp_tp) (Some act_tp)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "In a kind union, type mismatch on kind name:" PP.<+> S.ppText k
  PP.$$ PP.nest 2 (PP.text "Expected type:") PP.<+> PP.nest 6 (ppTypeRepr exp_tp)
  PP.$$ PP.nest 2 (PP.text "  Actual type:") PP.<+> PP.nest 6 (ppTypeRepr act_tp)
ppTypeError fp (NoSuchFieldError (L p f) (L _ x) (Some tp)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "No such field" PP.<+> ppQText f PP.<+> PP.text "of expression:"
  PP.$$ PP.nest 2 (S.ppExpr x PP.<+> PP.text ":" PP.<+> ppTypeRepr tp)
ppTypeError fp (StructLiteralTypeError (L p tp)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Type given for a struct literal is not a struct:" PP.<+> S.ppType tp
ppTypeError fp (StructLiteralNameMismatchError (L p s1) s2) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Field in struct literal" PP.<+> ppQText s1
  PP.<+> PP.text "does not match the exepcted field" PP.<+> ppQText s2
ppTypeError fp (StructLiteralLengthError p (Some ftps) fvs) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Struct literal should have" PP.<+> PP.int (sizeInt . size $ ftps)
  PP.<+> PP.text "fields, but has" PP.<+> PP.int (length fvs)
ppTypeError fp (StructLiteralAbstractTypeError p _) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Cannot construct a literal of a struct type containg an abstract type."
ppTypeError fp (KindNameNotInScope (L p k)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Kind name" PP.<+> ppQText k PP.<+> PP.text "not in scope."
ppTypeError fp (FunctionNameNotInScope (L p fn)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Function name" PP.<+> ppQText fn PP.<+> PP.text "not in scope."
ppTypeError fp (FieldNameNotInScope (L p f)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Field name" PP.<+> ppQText f PP.<+> PP.text "not in scope."
ppTypeError fp (OtherNameNotInScope (L p nm)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Identifier" PP.<+> ppQText nm PP.<+> PP.text "not in scope."
ppTypeError fp (KindNameAlreadyDefined (L p k)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "A kind with name" PP.<+> ppQText k PP.<+> PP.text "is already defined."
ppTypeError fp (FunctionNameAlreadyDefined (L p fn)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "A function with name" PP.<+> ppQText fn PP.<+> PP.text "is already defined."
ppTypeError fp (FieldNameAlreadyDefined (L p f)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "A field with name" PP.<+> ppQText f PP.<+> PP.text "is already in scope."
ppTypeError fp (FunctionArgLengthError (L p fn) (Some (FunctionTypeRepr{..})) args) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Function" PP.<+> ppQText fn
  PP.<+> PP.text "expects" PP.<+> PP.int (sizeInt . size $ functionArgTypes)
  PP.<+> PP.text "arguments, but was given" PP.<+> PP.int (length args)
ppTypeError fp (UnexpectedSelfError p) =
  PP.text (errorPrefix fp p) PP.<+> PP.text "Unexpected 'self'."
ppTypeError fp (InternalError p str) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Internal error!" PP.$$ PP.nest 2 (S.ppText str)

ppQText :: Text -> PP.Doc
ppQText = PP.quotes . S.ppText

ppTypeWarning :: FilePath -> TypeWarning -> PP.Doc
ppTypeWarning fp (EnumNameNotInScope (L p fn)) =
  PP.text (errorPrefix fp p)
  PP.<+> PP.text "Warning: Enum name" PP.<+> S.ppText fn PP.<+> PP.text "not in scope."

data SomeTypeOrString :: * where
  SomeType   :: T.TypeRepr tp -> SomeTypeOrString
  TypeString :: Text -> SomeTypeOrString
deriving instance Show SomeTypeOrString

ppSomeTypeOrString :: SomeTypeOrString -> PP.Doc
ppSomeTypeOrString (SomeType tp) = ppTypeRepr tp
ppSomeTypeOrString (TypeString s) = S.ppText s
