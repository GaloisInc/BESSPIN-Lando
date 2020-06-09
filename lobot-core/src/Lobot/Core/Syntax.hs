{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lobot.Core.Syntax
Description : The untyped AST for the Lobot Lobot sublanguage.
Copyright   : (c) Ben Selfridge, Matthew Yacavone, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the untyped AST for the Lobot sublanguage of Lando. When
parsing a concrete syntax for Lobot, this is the data type we target. Then, this
type passes through the type checker to produce the typed AST in
'Lobot.Core.Kind'.
-}
module Lobot.Core.Syntax where

import Data.Text (Text)

data KindDecl = KindDecl { kindDeclName :: Text
                         , kindDeclType :: Type
                         , kindDeclConstraints :: [Expr]
                         } deriving Show

data Type = BoolType
          | IntType
          | EnumType [Text]
          | SetType [Text]
          | StructType [(Text, Type)]
          | KindNames [Text]
          deriving Show

data Expr = LiteralExpr Literal
          | SelfExpr
          | FieldExpr Expr Text -- ^ struct.field
          | EqExpr Expr Expr
          | LteExpr Expr Expr
          | MemberExpr Expr Expr
          | ImpliesExpr Expr Expr
          | NotExpr Expr
          | IsInstanceExpr Expr Type -- ^ expr : type (in a constraint)
          deriving Show

data Literal = BoolLit Bool
             | IntLit Integer
             | EnumLit Text
             | SetLit [Text]
             | StructLit [(Text, Expr)]
             deriving Show
