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

Some examples of Lobot untyped syntax. After running `typeCheck`, all the
examples given here are identical to those in `Kind.Examples`.
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

abc :: KindDecl
abc = KindDecl { kindDeclName = "abc"
               , kindDeclType = SetType ["A","B","C"]
               , kindDeclConstraints = []
               }

abc_1 :: KindDecl
abc_1 = KindDecl { kindDeclName = "abc_1"
                 , kindDeclType = KindNames ["abc"]
                 , kindDeclConstraints =
                   [ MemberExpr
                     (LiteralExpr (EnumLit "A"))
                     SelfExpr
                   ]
                 }

abc_2 :: KindDecl
abc_2 = KindDecl { kindDeclName = "abc_2"
                 , kindDeclType = KindNames ["abc"]
                 , kindDeclConstraints =
                   [ ImpliesExpr
                     (MemberExpr
                      (LiteralExpr (EnumLit "A"))
                      SelfExpr)
                     (MemberExpr
                      (LiteralExpr (EnumLit "C"))
                      SelfExpr)
                   ]
                 }

abc_3 :: KindDecl
abc_3 = KindDecl { kindDeclName = "abc_3"
                 , kindDeclType = KindNames ["abc_1", "abc_2"]
                 , kindDeclConstraints = []
                 }

riscv :: KindDecl
riscv = KindDecl { kindDeclName = "riscv"
                 , kindDeclType =
                   StructType [ ("reg_width", EnumType ["RV32","RV64"])
                              , ("xlen", IntType)
                              , ("exts", SetType ["M","A","F","D","C"])
                              , ("privs", SetType ["PrivM","PrivS","PrivU"])
                              , ("vm", EnumType ["SVNone","SV32","SV39","SV48"])
                              ]
                 , kindDeclConstraints =
                   [ ImpliesExpr
                     (EqExpr
                      (FieldExpr SelfExpr "reg_width")
                      (LiteralExpr (EnumLit "RV32")))
                     (EqExpr
                      (FieldExpr SelfExpr "xlen")
                      (LiteralExpr (IntLit 32)))
                   , ImpliesExpr
                     (EqExpr
                      (FieldExpr SelfExpr "reg_width")
                      (LiteralExpr (EnumLit "RV64")))
                     (EqExpr
                      (FieldExpr SelfExpr "xlen")
                      (LiteralExpr (IntLit 64)))
                   , ImpliesExpr
                     (MemberExpr
                      (LiteralExpr (EnumLit "D"))
                      (FieldExpr SelfExpr "exts"))
                     (MemberExpr
                      (LiteralExpr (EnumLit "F"))
                      (FieldExpr SelfExpr "exts"))
                   , MemberExpr
                     (LiteralExpr (EnumLit "PrivM"))
                     (FieldExpr SelfExpr "privs")
                   , ImpliesExpr
                     (MemberExpr
                      (LiteralExpr (EnumLit "PrivS"))
                      (FieldExpr SelfExpr "privs"))
                     (MemberExpr
                      (LiteralExpr (EnumLit "PrivU"))
                      (FieldExpr SelfExpr "privs"))
                   , ImpliesExpr
                     (MemberExpr
                      (LiteralExpr (EnumLit "PrivS"))
                      (FieldExpr SelfExpr "privs"))
                     (NotExpr
                      (EqExpr
                       (FieldExpr SelfExpr "vm")
                       (LiteralExpr (EnumLit "SVNone"))))
                   , ImpliesExpr
                     (NotExpr
                      (MemberExpr
                       (LiteralExpr (EnumLit "PrivS"))
                       (FieldExpr SelfExpr "privs")))
                     (EqExpr
                      (FieldExpr SelfExpr "vm")
                      (LiteralExpr (EnumLit "SVNone")))
                   ]
                 }

bluespec_build :: KindDecl
bluespec_build = KindDecl { kindDeclName = "bluespec_build"
                          , kindDeclType =
                            StructType [ ("riscv", KindNames ["riscv"])
                                       , ("sim", EnumType ["Bluesim","IVerilog","Verilator"])
                                       , ("tv", BoolType)
                                       ]
                          , kindDeclConstraints =
                            [ MemberExpr
                              (FieldExpr (FieldExpr SelfExpr "riscv") "vm")
                              (LiteralExpr (SetLit [ "SVNone"
                                                   , "SV32"
                                                   , "SV39"
                                                   ]))
                            ]
                          }
