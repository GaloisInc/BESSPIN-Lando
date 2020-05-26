{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Lando.Core.Syntax where

data FieldDecl = FieldDecl FieldName FieldType

data FieldName = FieldName String

data FieldType = BaseType BaseType

data BaseType = IntType
              | EnumType [String]
              | SubsetType BaseType

data Expr = Literal Literal
          | ExprField String
          | Equal Expr Expr
          | Set [Expr]

data Kind = Kind { kindName :: String
                 , kindParent :: Maybe String
                 , kindFields :: [FieldDecl]
                 , kindConstraints :: [Expr]
                 }

data Instance = Instance { instanceName :: String
                         , instanceKind :: String
                         , instanceConstraints :: [Expr]
                         }
