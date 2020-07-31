{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lobot.Syntax
Description : The untyped AST for the Lobot sublanguage.
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
  , Check(..)
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
          -- | A check is just a kind, with additional requirements that must
          -- hold for any instance.
          | CheckDecl Check
          deriving (Show, Eq)

data Kind = Kind { kindName :: LText
                 , kindType :: LType
                 , kindConstraints :: [LExpr]
                 } deriving (Show, Eq)

data Check = Check { checkName :: LText
                   , checkFields :: [(LText, LType)]
                   , checkConstraints :: [LExpr]
                   , checkRequirements :: [LExpr]
                   } deriving (Show, Eq)

data FunctionType = FunType LText [LType] LType
                  deriving (Show, Eq)

data Type = BoolType
          | IntType
          | EnumType [Text]
          | SetType LType
          | StructType [(LText, LType)]
          | KindNames [LText]
          deriving (Show, Eq)

data Expr = LiteralExpr LLiteral
          | SelfExpr
          | VarExpr LText
          | FieldExpr LExpr LText -- ^ struct.field
          | ApplyExpr LText [LExpr]
          | IsInstanceExpr LExpr LType -- ^ expr : type (in a constraint)
          | EqExpr LExpr LExpr
          -- Integer operations
          | LteExpr LExpr LExpr
          | LtExpr LExpr LExpr
          | GteExpr LExpr LExpr
          | GtExpr LExpr LExpr
          | PlusExpr LExpr LExpr
          | MinusExpr LExpr LExpr
          | TimesExpr LExpr LExpr
          | ModExpr LExpr LExpr
          | DivExpr LExpr LExpr
          | NegExpr LExpr
          -- Enum operations
          | MemberExpr LExpr LExpr
          | NotMemberExpr LExpr LExpr
          -- Boolean operations
          | AndExpr LExpr LExpr
          | OrExpr LExpr LExpr
          | XorExpr LExpr LExpr
          | ImpliesExpr LExpr LExpr
          | IffExpr LExpr LExpr
          | NotExpr LExpr
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
