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
  , ExprP(..)
  , Expr
  , LExprP
  , LExpr
  , LiteralP(..)
  , Literal
  , LLiteralP
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

data ExprP t = LiteralExpr (LLiteralP t)
             | SelfExpr
             | VarExpr LText
          -- | SelfFieldExpr LText
             | FieldExpr (LExprP t) LText -- ^ struct.field
             | ApplyExpr LText [LExprP t]
             | EqExpr (LExprP t) (LExprP t)
             | LteExpr (LExprP t) (LExprP t)
             | PlusExpr (LExprP t) (LExprP t)
             | MemberExpr (LExprP t) (LExprP t)
             | ImpliesExpr (LExprP t) (LExprP t)
             | NotExpr (LExprP t)
             | IsInstanceExpr (LExprP t) t -- ^ expr : type (in a constraint)
             deriving (Show, Eq)

type Expr = ExprP LType

data LiteralP t = BoolLit Bool
                | IntLit Integer
                | EnumLit LText
                | SetLit [LText]
                | StructLit t [(LText, LLiteralP t)]
                deriving (Show, Eq)

type Literal = LiteralP LType

type LType       = Loc Type
type LExprP t    = Loc (ExprP t)
type LExpr       = Loc Expr
type LLiteralP t = Loc (LiteralP t)
type LLiteral    = Loc Literal
type LText       = Loc Text
