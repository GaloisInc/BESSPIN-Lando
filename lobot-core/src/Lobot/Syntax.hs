{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lobot.Syntax
Description : The untyped AST for the Lobot Lobot sublanguage.
Copyright   : (c) Ben Selfridge, Matthew Yacavone, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the untyped AST for the Lobot sublanguage of Lando. When
parsing a concrete syntax for Lobot, this is the data type we target. Then, this
type passes through the type checker to produce the typed AST in
'Lobot.Kind'.
-}
module Lobot.Syntax
  ( Loc(..)
  , KindDecl(..)
  , Type(..)
  , LType
  , Expr(..)
  , LExpr
  , Literal(..)
  , LLiteral
  , LText
  ) where

import Data.Text (Text)

import Lobot.Lexer

data KindDecl = KindDecl { kindDeclName :: Text
                         , kindDeclType :: LType
                         , kindDeclConstraints :: [LExpr]
                         } deriving Show

data Type = BoolType
          | IntType
          | EnumType [Text]
          | SetType [Text]
          | StructType [(LText, LType)]
          | KindNames [LText]
          deriving Show

data Expr = LiteralExpr LLiteral
          | SelfExpr
          | SelfFieldExpr LText
          | FieldExpr LExpr LText -- ^ struct.field
          | ApplyExpr LText [LExpr]
          | EqExpr LExpr LExpr
          | LteExpr LExpr LExpr
          | PlusExpr LExpr LExpr
          | MemberExpr LExpr LExpr
          | ImpliesExpr LExpr LExpr
          | NotExpr LExpr
          | IsInstanceExpr LExpr LType -- ^ expr : type (in a constraint)
          deriving Show

data Literal = BoolLit Bool
             | IntLit Integer
             | EnumLit LText
             | SetLit [LText]
             | StructLit [(LText, LLiteral)]
             deriving Show

type LType    = Loc Type
type LExpr    = Loc Expr
type LLiteral = Loc Literal
type LText    = Loc Text
