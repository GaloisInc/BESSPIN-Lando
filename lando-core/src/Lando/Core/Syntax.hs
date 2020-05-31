{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Lando.Core.Syntax
Description : The untyped AST for the Lando LOBOT sublanguage.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the untyped AST for the LOBOT sublanguage of lando. When
parsing a concrete syntax for Lando, this is the data type we target. Then, this
type passes through the type checker to produce the typed AST in
'Lando.Core.Kind'.
-}
module Lando.Core.Syntax where

data KindDecl = RootDecl RootKind
              | DerivedDecl DerivedKind

data RootKind = RootKind { rootKindName :: String
                         , rootKindField :: [Field]
                         , rootKindConstraints :: [Expr]
                         }

data DerivedKind = DerivedKind { derivedKindName :: String
                               , derivedParentKindName :: String
                               , derivedConstraints :: [Expr]
                               }

data Field = Field { fieldName :: String
                   , fieldType :: Type
                   }

data Type = BoolType
          | IntType
          | EnumType [String]
          | SetType Type
          | KindType String

data Expr = LiteralExpr Literal
          | SelfExpr
          | FieldExpr Expr String -- ^ kind.field
          | EqExpr Expr Expr
          | LteExpr Expr Expr
          | MemberExpr Expr Expr
          | ImpliesExpr Expr Expr
          | NotExpr Expr
          | IsInstance String

data Literal = BoolLit Bool
             | IntLit Integer
             | EnumLit String
             | SetLit [Literal]
