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
  , CoupleType
  , couple_kind
  , straight_couple_kind
  , teenage_couple_kind
    -- * Example 3
  , RegWidthType, ExtsType, PrivType, VMType, RISCVType
  , riscv
  , SimType, BluespecBuildType
  , bluespec_build
  ) where

import Data.List.NonEmpty
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Lando.Core.Kind

type ABCType = '["A", "B", "C"]

type FooType = '[ '("abc", SetType ABCType) ]

abc_type :: List SymbolRepr ABCType
abc_type = knownRepr

-- |
-- @
-- kind abc_kind
--   with abc : subset {A, B, C}
-- @
abc_kind :: Kind FooType
abc_kind = Kind { kindName = "abc"
                , kindFields = knownRepr
                , kindConstraints = []
                }

-- |
-- @
-- kind abc_kind_1 of abc_kind where A in abc
-- @
abc_kind_1 :: Kind FooType
abc_kind_1 = derivedKind (abc_kind :| []) "abc_kind_1"
                  [ MemberExpr
                    (LiteralExpr (EnumLit abc_type index0))
                    (FieldExpr SelfExpr index0)
                  ]

-- |
-- @
-- kind abc_kind_2 of abc_kind where (A in abc) => (C in abc)
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
-- kind abc_kind2 of abc_kind_1 abc_kind_2
-- @
abc_kind_3 :: Kind FooType
abc_kind_3 = derivedKind
  (abc_kind_1 :| [abc_kind_2])
  "abc_kind_3"
  []

type SexType = '["Male", "Female"]

_sex_type :: List SymbolRepr SexType
_sex_type = knownRepr

type PersonType = '[ '("age", IntType)
                   , '("sex", EnumType SexType)
                   ]

-- |
-- @
-- kind person
--   with age : int
--        sex : {Male, Female}
--   where age >= 0
-- @
person_kind :: Kind PersonType
person_kind = Kind { kindName = "person"
                   , kindFields = knownRepr
                   , kindConstraints =
                       [ LteExpr
                         (LiteralExpr (IntLit 0))
                         (FieldExpr SelfExpr index0)
                       ]
                   }

-- |
-- @
-- kind teenager of person
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

type CoupleType = '[ '("person1", KindType PersonType)
                   , '("person2", KindType PersonType)
                   ]

-- |
-- @
-- kind couple
--   with person1 : person,
--        person2 : person
-- @
couple_kind :: Kind CoupleType
couple_kind = Kind { kindName = "couple"
                   , kindFields = knownRepr
                   , kindConstraints = []
                   }

-- |
-- @
-- kind straight_couple of couple
--   where not (person1.sex = person2.sex)
-- @
straight_couple_kind :: Kind CoupleType
straight_couple_kind = derivedKind (couple_kind :| []) "straight_couple"
                       [ NotExpr
                         (EqExpr
                          (FieldExpr (FieldExpr SelfExpr index0) index1)
                          (FieldExpr (FieldExpr SelfExpr index1) index1))
                       ]

-- |
-- @ kind teenage_couple of couple
--     where person1 : teenager,
--           person2 : teenager
-- @
teenage_couple_kind :: Kind CoupleType
teenage_couple_kind = liftConstraints index0 teenager_kind $
                      liftConstraints index1 teenager_kind $
                      derivedKind (couple_kind :| []) "teenage_couple" []

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

type RISCVType = '[ '("reg_width", EnumType RegWidthType),
                    '("xlen", IntType),
                    '("exts", SetType ExtsType),
                    '("privs", SetType PrivType),
                    '("vm", EnumType VMType)
                  ]

-- |
-- @
-- kind riscv
--   with reg_width : {RV32, RV64},
--        xlen : int,
--        exts : subset {M, A, F, D, C},
--        privs : subset {PrivM, PrivS, PrivU},
--        vm : {SVNone, SV32, SV39, SV48}
--   where (reg_width = RV32) => (xlen = 32),
--         (reg_width = RV64) => (xlen = 64),
--         (D in exts) => (F in exts),
--         PrivM in privs,
--         (PrivS in privs) => (PrivU in privs),
--         (PrivS in privs) => (not (vm = SVNone)),
--         (not (PrivS in privs)) => (vm = SVNone)
-- @
riscv :: Kind RISCVType
riscv = Kind { kindName = "riscv"
             , kindFields = knownRepr
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

type BluespecBuildType = '[ '("riscv", KindType RISCVType)
                          , '("sim", EnumType SimType)
                          , '("tv", BoolType)
                          ]

-- |
-- @
-- kind bluespec_build
--   with riscv : riscv,
--        sim : {Bluesim, IVerilog, Verilator}
--        tv : bool
--   where riscv.vm in {SVNone, SV32, SV39}
-- @
bluespec_build :: Kind BluespecBuildType
bluespec_build = liftConstraints index0 riscv k
  where k = Kind { kindName = "bluespec_build"
                 , kindFields = knownRepr
                 , kindConstraints =
                   [ MemberExpr
                     (FieldExpr (FieldExpr SelfExpr index0) index4)
                     (LiteralExpr (SetLit vm_type [ Some index0
                                                  , Some index1
                                                  , Some index2
                                                  ]))
                   ]
                 }
