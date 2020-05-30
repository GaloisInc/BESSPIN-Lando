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

-- | Untyped AST for a LOBOT Kind.
data Kind = Kind { kindName :: String
                 , kindParent :: Maybe String
                 , kindFields :: [FieldDecl]
                 , kindConstraints :: [Expr]
                 }

-- | Untyped field declaration.
data FieldDecl = FieldDecl FieldName FieldType

-- | Name of a field.
data FieldName = FieldName String

data FieldType = IntType
               | EnumType [String]
               | SubsetType BaseType

data Expr = Literal Literal
          | ExprField String
          | Equal Expr Expr
          | Set [Expr]

data Instance = Instance { instanceName :: String
                         , instanceKind :: String
                         , instanceConstraints :: [Expr]
                         }
