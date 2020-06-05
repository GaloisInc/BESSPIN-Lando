{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

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
                , kindFields = FieldRepr knownSymbol (SetRepr abc_type) :< Nil
                , kindConstraints = []
                }

-- |
-- @
-- kind abc_kind_1 of abc_kind
--   with abc : subset {A, B, C}
--   where A in abc
-- @
abc_kind_1 :: Kind FooType
abc_kind_1 = derivedKind (abc_kind :| []) "abc_kind_1"
                  [ MemberExpr
                    (LiteralExpr (EnumLit abc_type index0))
                    (FieldExpr SelfExpr index0)
                  ]

-- |
-- @
-- kind abc_kind_2 of abc_kind
--   with abc : subset {A, B, C}
--   where (A in abc) => (C in abc)
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

sex_type :: List SymbolRepr SexType
sex_type = knownRepr

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
                   , kindFields = FieldRepr knownSymbol IntRepr :<
                                  FieldRepr knownSymbol (EnumRepr sex_type) :< Nil
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

type RegWidthType = '["RV32", "RV64"]
reg_width_type :: List SymbolRepr RegWidthType
reg_width_type = knownSymbol :< knownSymbol :< Nil

type ExtsType = '["M", "A", "F", "D", "C"]
exts_type :: List SymbolRepr ExtsType
exts_type = knownRepr

type RISCVType = '[ '("reg_width", EnumType RegWidthType),
                    '("exts", SetType ExtsType)
                  ]

-- |
-- @
-- kind riscv
--   with reg_width : {RV32, RV64},
--        exts : subset {M, A, F, D, C}
--   where (D in exts) => (F in exts)
-- @
riscv :: Kind RISCVType
riscv = Kind { kindName = "riscv"
             , kindFields = FieldRepr knownSymbol (EnumRepr reg_width_type) :<
                            FieldRepr knownSymbol (SetRepr exts_type) :< Nil
             , kindConstraints =
               [ ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit exts_type index3))
                  (FieldExpr SelfExpr index1))
                 (MemberExpr
                  (LiteralExpr (EnumLit exts_type index2))
                  (FieldExpr SelfExpr index1))
               ]
             }

type SimType = '[ "Bluesim", "IVerilog", "Verilator" ]
sim_type :: List SymbolRepr SimType
sim_type = knownRepr

type BluespecBuildType = '[ '("riscv", KindType RISCVType)
                          , '("sim", EnumType SimType)
                          , '("INCLUDE_TANDEM_VERIF", BoolType)
                          ]

-- |
-- @
-- kind bluespec_build
--   with riscv : riscv,
--        sim : {Bluesim, IVerilog, Verilator}
-- @
bluespec_build :: Kind BluespecBuildType
bluespec_build = liftConstraints k riscv index0
  where k = Kind { kindName = "bluespec_build"
                 , kindFields = FieldRepr knownSymbol (KindRepr (kindFields riscv)) :<
                                FieldRepr knownSymbol (EnumRepr sim_type) :<
                                FieldRepr knownSymbol BoolRepr :< Nil
                 , kindConstraints = []
                 }
