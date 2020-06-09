{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lobot.Core.Syntax.Examples
Description : Examples of untyped syntax.
Copyright   : (c) Matthew Yacavone, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

Some examples of Lobot untyped syntax.
-}
module Lobot.Core.Syntax.Examples where

import Lobot.Core.Syntax

-- Examples from Lobot.Core.Kind.Examples

posint :: KindDecl
posint = KindDecl { kindDeclName = "posint"
                  , kindDeclType = IntType
                  , kindDeclConstraints =
                    [ LteExpr (LiteralExpr (IntLit 0)) SelfExpr
                    ]
                  }

unique_posint_pair :: KindDecl
unique_posint_pair = KindDecl { kindDeclName = "unique_posint_pair"
                              , kindDeclType =
                                StructType [ ("x", KindNames ["posint"])
                                           , ("y", KindNames ["posint"]) ]
                              , kindDeclConstraints =
                                [ NotExpr
                                  (EqExpr
                                   (FieldExpr SelfExpr "x")
                                   (FieldExpr SelfExpr "y"))
                                , LteExpr
                                  (FieldExpr SelfExpr "x")
                                  (FieldExpr SelfExpr "y")
                                ]
                              }

-- Kind {
-- kindName = "unique_posint_pair",
-- kindType = StructRepr [FieldRepr {fieldName = x, fieldType = IntRepr}, FieldRepr {fieldName = y, fieldType = IntRepr}], 
-- kindFunctionEnv = [],
-- kindConstraints = [NotExpr (EqExpr (FieldExpr SelfExpr 0) (FieldExpr SelfExpr 1)),LteExpr (FieldExpr SelfExpr 0) (FieldExpr SelfExpr 1),LteExpr (LiteralExpr (IntLit 0)) (FieldExpr SelfExpr 1),LteExpr (LiteralExpr (IntLit 0)) (FieldExpr SelfExpr 0)]}
-- 
-- Kind {
-- kindName = "unique_posint_pair",
-- kindFunctionEnv = [],
-- kindConstraints = [NotExpr (EqExpr (FieldExpr SelfExpr 0) (FieldExpr SelfExpr 1)),LteExpr (FieldExpr SelfExpr 0) (FieldExpr SelfExpr 1)]}