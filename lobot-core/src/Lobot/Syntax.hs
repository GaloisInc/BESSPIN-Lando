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
Maintainer  : myac@galoicom
Stability   : experimental
Portability : POSIX

This module defines the untyped AST for the Lobot sublanguage of Lando. When
parsing a concrete syntax for Lobot, this is the data type we target. Then, this
type passes through the type checker to produce the typed AST in
'Lobot.Kind'.
-}
module Lobot.Syntax
  ( Loc(..)
  , Decl(..)
  , Kind(..)
  , FunctionType(..)
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

data Decl = KindDecl Kind
          | TypeSynDecl LText LType
          | AbsTypeDecl LText
          | AbsFunctionDecl LText FunctionType
          deriving (Show, Eq)

data Kind = Kind { kindName :: LText
                 , kindType :: LType
                 , kindConstraints :: [LExpr]
                 } deriving (Show, Eq)

data FunctionType = FunType LText [LType] LType
                  deriving (Show, Eq)

data Type = BoolType
          | IntType
          | EnumType [Text]
          | SetType [Text]
          | StructType [(LText, LType)]
          | KindNames [LText]
          deriving (Show, Eq)

data Expr = LiteralExpr LLiteral
          | SelfExpr
          | VarExpr LText
          | FieldExpr LExpr LText -- ^ struct.field
          | ApplyExpr LText [LExpr]
          | EqExpr LExpr LExpr
          | LteExpr LExpr LExpr
          | PlusExpr LExpr LExpr
          | MemberExpr LExpr LExpr
          | ImpliesExpr LExpr LExpr
          | NotExpr LExpr
          | IsInstanceExpr LExpr LType -- ^ expr : type (in a constraint)
          deriving (Show, Eq)

data Literal = BoolLit Bool
             | IntLit Integer
             | EnumLit LText
             | SetLit [LText]
             | StructLit (Maybe LType) [(LText, LLiteral)]
             deriving (Show, Eq)

type LType       = Loc Type
type LExpr       = Loc Expr
type LLiteral    = Loc Literal
