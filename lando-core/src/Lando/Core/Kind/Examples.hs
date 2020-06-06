{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lando.Core.Kind.Examples
  ( -- * Example 1
    ABCType, FooType
  , abc_kind
  , abc_kind_1
  , abc_kind_2
  , abc_kind_3
    -- * Example 2
  , SexType, PersonType
  , person_kind
  , teenager_kind
    -- * Example 3
  , RegWidthType, ExtsType, RISCVType
  , riscv
  , SimType, BluespecBuildType
  , bluespec_build
  ) where

import Data.List.NonEmpty
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.SymbolRepr
import Lando.Core.Kind

type ABCType = '["A", "B", "C"]

type FooType = StructType '[ '("abc", SetType ABCType) ]

abc_type :: List SymbolRepr ABCType
abc_type = knownRepr

-- |
-- @
-- abc_kind kind of struct
--   with abc : subset {A, B, C}
-- @
abc_kind :: Kind FooType
abc_kind = Kind { kindName = "abc"
                , kindType = knownRepr
                , kindConstraints = []
                }

-- |
-- @
-- abc_kind_1 kind of abc_kind where A in abc
-- @
abc_kind_1 :: Kind FooType
abc_kind_1 = derivedKind (abc_kind :| []) "abc_kind_1"
                  [ MemberExpr
                    (LiteralExpr (EnumLit abc_type index0))
                    (FieldExpr SelfExpr index0)
                  ]

-- |
-- @
-- abc_kind_2 kind of of abc_kind where (A in abc) => (C in abc)
-- @
abc_kind_2 :: Kind FooType
abc_kind_2 = derivedKind (abc_kind :| []) "abc_kind_2"
             [ ImpliesExpr
               (MemberExpr
                (LiteralExpr (EnumLit abc_type index0))
                (FieldExpr SelfExpr index0))
               (MemberExpr
                (LiteralExpr (EnumLit abc_type index2))
                (FieldExpr SelfExpr index0))
             ]

-- |
-- @
-- abc_kind2 kind of abc_kind_1 abc_kind_2
-- @
abc_kind_3 :: Kind FooType
abc_kind_3 = derivedKind
  (abc_kind_1 :| [abc_kind_2])
  "abc_kind_3"
  []

type SexType = '["Male", "Female"]

_sex_type :: List SymbolRepr SexType
_sex_type = knownRepr

type PersonType = StructType '[ '("age", IntType)
                              , '("sex", EnumType SexType)
                              ]

-- |
-- @
-- person kind of struct
--   with age : int
--        sex : {Male, Female}
--   where age >= 0
-- @
person_kind :: Kind PersonType
person_kind = Kind { kindName = "person"
                   , kindType = knownRepr
                   , kindConstraints =
                       [ LteExpr
                         (LiteralExpr (IntLit 0))
                         (FieldExpr SelfExpr index0)
                       ]
                   }

-- |
-- @
-- teenager kind of person
--   where 13 <= age,
--         age <= 19
-- @
teenager_kind :: Kind PersonType
teenager_kind = derivedKind (person_kind :| []) "teenager"
                [ LteExpr
                  (LiteralExpr (IntLit 13))
                  (FieldExpr SelfExpr index0)
                , LteExpr
                  (FieldExpr SelfExpr index0)
                  (LiteralExpr (IntLit 19))
                ]

type RegWidthType = '["RV32", "RV64"]
reg_width_type :: List SymbolRepr RegWidthType
reg_width_type = knownRepr

type ExtsType = '["M", "A", "F", "D", "C"]
exts_type :: List SymbolRepr ExtsType
exts_type = knownRepr

type PrivType = '["PrivM", "PrivS", "PrivU"]
priv_type :: List SymbolRepr PrivType
priv_type = knownRepr

type VMType = '["SVNone", "SV32", "SV39", "SV48"]
vm_type :: List SymbolRepr VMType
vm_type = knownRepr

type RISCVType = StructType '[ '("reg_width", EnumType RegWidthType),
                               '("xlen", IntType),
                               '("exts", SetType ExtsType),
                               '("privilege", SetType PrivType),
                               '("vm", EnumType VMType)
                             ]

-- |
-- @
-- riscv kind of struct
--   with reg_width : {RV32, RV64},
--        xlen : int,
--        exts : subset {M, A, F, D, C},
--        privilege : subset {PrivM, PrivS, PrivU},
--        vm : {SVNone, SV32, SV39, SV48}
--   where (reg_width = RV32) => (xlen = 32),
--         (reg_width = RV64) => (xlen = 64),
--         (D in exts) => (F in exts),
--         PrivM in privilege,
--         (PrivS in privilege) => (PrivU in privilege),
--         (PrivS in privilege) => (not (vm = SVNone)),
--         (not (PrivS in privilege)) => (vm = SVNone)
-- @
riscv :: Kind RISCVType
riscv = Kind { kindName = "riscv"
             , kindType = knownRepr
             , kindConstraints =
               [ ImpliesExpr
                 (EqExpr
                  (FieldExpr SelfExpr index0)
                  (LiteralExpr (EnumLit reg_width_type index0)))
                 (EqExpr
                  (FieldExpr SelfExpr index1)
                  (LiteralExpr (IntLit 32)))
               , ImpliesExpr
                 (EqExpr
                  (FieldExpr SelfExpr index0)
                  (LiteralExpr (EnumLit reg_width_type index1)))
                 (EqExpr
                  (FieldExpr SelfExpr index1)
                  (LiteralExpr (IntLit 64)))
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit exts_type index3))
                  (FieldExpr SelfExpr index2))
                 (MemberExpr
                  (LiteralExpr (EnumLit exts_type index2))
                  (FieldExpr SelfExpr index2))
               , MemberExpr
                 (LiteralExpr (EnumLit priv_type index0))
                 (FieldExpr SelfExpr index3)
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit priv_type index1))
                  (FieldExpr SelfExpr index3))
                 (MemberExpr
                  (LiteralExpr (EnumLit priv_type index2))
                  (FieldExpr SelfExpr index3))
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit priv_type index1))
                  (FieldExpr SelfExpr index3))
                 (NotExpr
                  (EqExpr
                   (FieldExpr SelfExpr index4)
                   (LiteralExpr (EnumLit vm_type index0))))
               , ImpliesExpr
                 (NotExpr
                  (MemberExpr
                   (LiteralExpr (EnumLit priv_type index1))
                   (FieldExpr SelfExpr index3)))
                 (EqExpr
                  (FieldExpr SelfExpr index4)
                  (LiteralExpr (EnumLit vm_type index0)))
               ]
             }

index4 :: Index (x0:x1:x2:x3:x4:r) x4
index4 = IndexThere index3

type SimType = '[ "Bluesim", "IVerilog", "Verilator" ]
_sim_type :: List SymbolRepr SimType
_sim_type = knownRepr

type BluespecBuildType = StructType '[ '("riscv", RISCVType)
                                     , '("sim", EnumType SimType)
                                     , '("INCLUDE_TANDEM_VERIF", BoolType)
                                     ]

-- |
-- @
-- bluespec_build kind of struct
--   with riscv : riscv,
--        sim : {Bluesim, IVerilog, Verilator}
-- @
bluespec_build :: Kind BluespecBuildType
bluespec_build = liftConstraints k riscv index0
  where k = Kind { kindName = "bluespec_build"
                 , kindType = knownRepr
                 , kindConstraints = []
                 }
