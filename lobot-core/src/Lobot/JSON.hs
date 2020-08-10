{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Lobot.JSON
Description : Conversion of Lobot types to/from JSON.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module provides functions for serializing and parsing JSON values.
-}
module Lobot.JSON
  ( -- * Types
    typeToJSON
  , typeFromJSON
    -- * Literals
  , literalToJSON
  , literalFromJSON
  ) where

import Lobot.Expr
import Lobot.Types
import Lobot.Utils

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
  AbsRepr s -> object [ "variant" .= symbolRepr s ]

typeFromJSON :: Value -> Result (Some TypeRepr)
typeFromJSON = parse typeParseJSON

typeParseJSON :: Value -> Parser (Some TypeRepr)
typeParseJSON = withObject "Some TypeRepr" $ \o -> do
  variant <- o .: "variant"
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
    _ -> return $ viewSome (Some . AbsRepr) (someSymbol variant)

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
                           , "fields" .= toListFC fieldInstToJSON flds
                           ]
  AbsLit s bs -> object [ "variant" .= symbolRepr s
                        , "value" .= T.decodeUtf8 bs
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
      someFlds <- traverse fieldInstParseJSON fldValues
      Some flds <- return $ fromList someFlds
      return $ Some (StructLit flds)
    _ -> do
      Some s <- return $ someSymbol variant
      bs <- T.encodeUtf8 <$> o .: "value"
      return $ Some (AbsLit s bs)

fieldInstToJSON :: FieldInst Literal ftp -> Value
fieldInstToJSON (FieldInst nm _ l) =
  object [ "name" .= symbolRepr nm
         , "value" .= literalToJSON l
         ]

fieldInstParseJSON :: Value -> Parser (Some (FieldInst Literal))
fieldInstParseJSON = withObject "Some (FieldInst Literal)" $ \o -> do
  nmTxt <- o .: "name"
  valValue <- o .: "value"
  Some nm <- return $ someSymbol nmTxt
  Some val <- literalParseJSON valValue
  return $ Some (FieldInst nm (literalType val) val)
