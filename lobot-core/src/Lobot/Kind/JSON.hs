{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Lobot.Kind.JSON
Description : Conversion of Lobot types to/from JSON.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module provides functions for serializing and parsing JSON values.
-}
module Lobot.Kind.JSON
  ( -- * Types
    typeToJSON
  , typeFromJSON
    -- * Literals
  , literalToJSON
  , literalFromJSON
  ) where

import Lobot.Kind
import Lobot.Types
import Lobot.Utils

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC

typeToJSON :: TypeRepr tp -> Value
typeToJSON tp = case tp of
  BoolRepr -> object [ "variant" .= T.pack "bool" ]
  IntRepr -> object [ "variant" .= T.pack "int" ]
  EnumRepr cs -> object [ "variant" .= T.pack "enum"
                        , "constructors" .= toListFC symbolRepr cs
                        ]
  SetRepr cs -> object [ "variant" .= T.pack "set"
                       , "constructors" .= toListFC symbolRepr cs
                       ]
  StructRepr flds -> object [ "variant" .= T.pack "struct"
                            , "fields" .= toListFC fieldToJSON flds
                            ]

typeFromJSON :: Value -> Result (Some TypeRepr)
typeFromJSON = parse typeParseJSON

typeParseJSON :: Value -> Parser (Some TypeRepr)
typeParseJSON = withObject "Some TypeRepr" $ \o -> do
  variant :: T.Text <- o .: "variant"
  case variant of
    "bool" -> return $ Some BoolRepr
    "int" -> return $ Some IntRepr
    "enum" -> do
      cTxts <- o .: "constructors"
      Some cs <- return $ fromList (someSymbol <$> cTxts)
      case isZeroOrGT1 (ctxSizeNat (size cs)) of
        Left Refl -> fail "empty constructor list for enum type"
        Right LeqProof -> return $ Some (EnumRepr cs)
    "set" -> do
      cTxts <- o .: "constructors"
      Some cs <- return $ fromList (someSymbol <$> cTxts)
      case isZeroOrGT1 (ctxSizeNat (size cs)) of
        Left Refl -> fail "empty constructor list for set type"
        Right LeqProof -> return $ Some (SetRepr cs)
    "struct" -> do
      fldValues <- o .: "fields"
      someFlds <- traverse fieldParseJSON fldValues
      Some flds <- return $ fromList someFlds
      return $ Some (StructRepr flds)
    _ -> fail $ "invalid type variant: " ++ T.unpack variant

fieldToJSON :: FieldRepr ftp -> Value
fieldToJSON (FieldRepr nm tp) = object [ "name" .= symbolRepr nm
                                       , "type" .= typeToJSON tp
                                       ]

fieldParseJSON :: Value -> Parser (Some FieldRepr)
fieldParseJSON = withObject "Some FieldRepr" $ \o -> do
  nmTxt <- o .: "name"
  tpValue <- o .: "type"
  Some nm <- return $ someSymbol nmTxt
  Some tp <- typeParseJSON tpValue
  return $ Some (FieldRepr nm tp)

literalToJSON :: Literal tp -> Value
literalToJSON tp = case tp of
  BoolLit b -> object [ "variant" .= T.pack "bool"
                      , "value" .= b
                      ]
  IntLit x -> object [ "variant" .= T.pack "int"
                     , "value" .= x
                     ]
  EnumLit cs i -> object [ "variant" .= T.pack "enum"
                         , "constructors" .= toListFC symbolRepr cs
                         , "value" .= indexVal i
                         ]
  SetLit cs i -> object [ "variant" .= T.pack "set"
                        , "constructors" .= toListFC symbolRepr cs
                        , "values" .= (viewSome indexVal <$> i)
                        ]
  StructLit flds -> object [ "variant" .= T.pack "struct"
                           , "fields" .= toListFC fieldLiteralToJSON flds
                           ]

literalFromJSON :: Value -> Result (Some Literal)
literalFromJSON = parse literalParseJSON

literalParseJSON :: Value -> Parser (Some Literal)
literalParseJSON = withObject "Some Literal" $ \o -> do
  variant :: T.Text <- o .: "variant"
  case variant of
    "bool" -> Some . BoolLit <$> o .: "value"
    "int" -> Some . IntLit <$> o .: "value"
    "enum" -> do
      cTxts <- o .: "constructors"
      Some cs <- return $ fromList (someSymbol <$> cTxts)
      case isZeroOrGT1 (ctxSizeNat (size cs)) of
        Left Refl -> fail "empty constructor list for enum literal"
        Right LeqProof -> do
          iVal <- o .: "value"
          case intIndex iVal (size cs) of
            Just (Some i) -> return $ Some (EnumLit cs i)
            Nothing -> fail $
              "invalid index for enum literal with " ++ show (size cs) ++
              "constructors: " ++ show iVal
    "set" -> do
      cTxts <- o .: "constructors"
      Some cs <- return $ fromList (someSymbol <$> cTxts)
      case isZeroOrGT1 (ctxSizeNat (size cs)) of
        Left Refl -> fail "empty constructor list for set literal"
        Right LeqProof -> do
          iVals :: [Int] <- o .: "values"
          case sequence (flip intIndex (size cs) <$> iVals) of
            Just is -> return $ Some (SetLit cs is)
            Nothing -> fail $
              "invalid indices for set literal with " ++ show (size cs) ++
              "constructors: " ++ show iVals
    "struct" -> do
      fldValues <- o .: "fields"
      someFlds <- traverse fieldLiteralParseJSON fldValues
      Some flds <- return $ fromList someFlds
      return $ Some (StructLit flds)
    _ -> fail $ "invalid type variant: " ++ T.unpack variant

fieldLiteralToJSON :: FieldLiteral ftp -> Value
fieldLiteralToJSON (FieldLiteral nm l) = object [ "name" .= symbolRepr nm
                                                , "value" .= literalToJSON l
                                                ]

fieldLiteralParseJSON :: Value -> Parser (Some FieldLiteral)
fieldLiteralParseJSON = withObject "Some FieldLiteral" $ \o -> do
  nmTxt <- o .: "name"
  valValue <- o .: "value"
  Some nm <- return $ someSymbol nmTxt
  Some val <- literalParseJSON valValue
  return $ Some (FieldLiteral nm val)
