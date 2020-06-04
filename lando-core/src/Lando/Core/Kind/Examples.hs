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
  , PiccoloBuildType
  , piccolo_build
  ) where

import Data.List.NonEmpty
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.SymbolRepr
import Lando.Core.Kind

type ABCType = '["A", "B", "C"]

type FooType = '[ '("abc", SetType ABCType ) ]

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
  "abc_kind_1_and_A_implies_C"
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

type PiccoloBuildType = '[ '("riscv", KindType RISCVType)
                         , '("sim", EnumType SimType)
                         ]
-- |
-- @
-- kind piccolo_build
--   with riscv : riscv,
--        sim : {Bluesim, IVerilog, Verilator}
-- @
piccolo_build :: Kind PiccoloBuildType
piccolo_build = liftConstraints k riscv index0
  where k = Kind { kindName = "piccolo_build"
                 , kindFields = FieldRepr knownSymbol (KindRepr (kindFields riscv)) :<
                                FieldRepr knownSymbol (EnumRepr sim_type) :< Nil
                 , kindConstraints = []
                 }

-- couple :: Kind '[ '("person1", KindType PersonType)
--                 , '("person2", KindType PersonType) ]
-- couple = Kind { kindName = "couple"
--               , kindFields = FieldRepr (knownSymbol @"person1") personRepr :<
--                              FieldRepr (knownSymbol @"person2") personRepr :< Nil
--               , kindConstraints = []
--               }
--   where personRepr = KindRepr (age :< sex :< Nil)

-- impossible_person :: Kind PersonType
-- impossible_person = derivedKind person "impossible_person "
--   [ EqExpr
--     (FieldExpr SelfExpr index1) -- sex
--     (LiteralExpr (EnumLit sex_type index0)) -- male
--   , EqExpr
--     (FieldExpr SelfExpr index1) -- sex
--     (LiteralExpr (EnumLit sex_type index1)) -- female
--   ]

-- male_person :: Kind PersonType
-- male_person = derivedKind person "male_person"
--   [ EqExpr
--     (FieldExpr SelfExpr index1) -- sex
--     (LiteralExpr (EnumLit sex_type index0)) -- male
--   ]

-- female_person :: Kind PersonType
-- female_person = person `addConstraints`
--   [ EqExpr
--     (FieldExpr SelfExpr index1) -- sex
--     (LiteralExpr (EnumLit index1 knownNat)) -- female
--   ]

-- child :: Kind PersonType
-- child = person `addConstraints`
--   [ LteExpr (FieldExpr SelfExpr index0) (LiteralExpr (IntLit 18))]

-- ben :: Instance PersonType
-- ben = Instance { instanceValues =
--                  FieldLiteral knownSymbol (IntLit 30) :<
--                  FieldLiteral knownSymbol (EnumLit sex_type index0) :< Nil
--                }

-- emmy :: Instance PersonType
-- emmy = Instance { instanceValues =
--                   FieldLiteral age (IntLit 8) :<
--                   FieldLiteral sex (EnumLit index1 knownNat) :< Nil
--                 }

-- archer :: Instance PersonType
-- archer = Instance { instanceValues =
--                     FieldLiteral age (IntLit 4) :<
--                     FieldLiteral sex (EnumLit index0 knownNat) :< Nil
--                   }


-- reg_width :: FieldRepr '("reg_width", EnumType '["RV32", "RV64"])
-- reg_width = FieldRepr
--   { fieldName = knownSymbol @"reg_width"
--   , fieldType = EnumRepr (knownSymbol @"RV32" :< knownSymbol @"RV64" :< Nil) knownNat
--   }

-- exts :: FieldRepr '("exts", SetType '["M", "A", "F", "D", "C"])
-- exts = FieldRepr
--   { fieldName = knownSymbol @"exts"
--   , fieldType = SetRepr (knownSymbol @"M" :<
--                          knownSymbol @"A" :<
--                          knownSymbol @"F" :<
--                          knownSymbol @"D" :<
--                          knownSymbol @"C" :< Nil)
--                 knownNat
--   }

-- riscv_with_m :: Kind RISCVType
-- riscv_with_m = riscv `addConstraints`
--   [ MemberExpr
--     (LiteralExpr (EnumLit index0 knownNat)) -- M
--     (FieldExpr SelfExpr index1) -- exts
--   ]

-- riscv_with_only_d :: Kind RISCVType
-- riscv_with_only_d = riscv `addConstraints`
--   [ EqExpr
--     (FieldExpr SelfExpr index1) -- exts
--     (LiteralExpr (SetLit [Some index3] knownNat)) -- D
--   ]

-- rv32i :: Instance RISCVType
-- rv32i = Instance { instanceValues =
--                    FieldLiteral reg_width (EnumLit index0 knownNat) :<
--                    FieldLiteral exts (SetLit [] knownNat) :<
--                    Nil
--                  }

-- rv32m :: Instance RISCVType
-- rv32m = Instance { instanceValues =
--                    FieldLiteral reg_width (EnumLit index0 knownNat) :<
--                    FieldLiteral exts (SetLit [Some index0] knownNat) :<
--                    Nil
--                  }

-- rv32d :: Instance RISCVType
-- rv32d = Instance { instanceValues =
--                    FieldLiteral reg_width (EnumLit index0 knownNat) :<
--                    FieldLiteral exts (SetLit [Some index3] knownNat) :<
--                    Nil
--                  }

-- rv64gc :: Instance RISCVType
-- rv64gc = Instance { instanceValues =
--                     FieldLiteral reg_width (EnumLit index1 knownNat) :<
--                     FieldLiteral exts (SetLit [ Some index0
--                                             , Some index1
--                                             , Some index2
--                                             , Some index3
--                                             , Some (IndexThere index3)
--                                             ] knownNat) :<
--                     Nil
--                   }

-- riscv_field :: FieldRepr '("riscv", KindType RISCVType)
-- riscv_field = FieldRepr { fieldName = knownSymbol @"riscv"
--                         , fieldType = KindRepr (reg_width :< exts :< Nil)
--                         }

-- sim :: FieldRepr '("sim", EnumType '["Bluesim", "IVerilog", "Verilator"])
-- sim = FieldRepr { fieldName = knownSymbol @"sim"
--                 , fieldType = EnumRepr (knownSymbol @"Bluesim" :<
--                                         knownSymbol @"IVerilog" :<
--                                         knownSymbol @"Verilator" :<
--                                         Nil)
--                               knownNat
--                 }

-- type PiccoloBuildType = '[ '("riscv", KindType RISCVType)
--                          , '("sim", EnumType '["Bluesim", "IVerilog", "Verilator"])
--                          ]

-- piccolo_build :: Kind PiccoloBuildType
-- piccolo_build = Kind { kindName = "piccolo_build"
--                      , kindFields = riscv_field :< sim :< Nil
--                      , kindConstraints =
--                          [ ImpliesExpr -- inherited from riscv
--                            (MemberExpr -- D in riscv.exts
--                             (LiteralExpr (EnumLit index3 knownNat)) -- riscv.D
--                             (FieldExpr -- riscv.exts
--                              (FieldExpr SelfExpr index0) -- riscv
--                              index1))
--                            (MemberExpr -- F in riscv.exts
--                             (LiteralExpr (EnumLit index2 knownNat)) -- riscv.F
--                             (FieldExpr -- riscv.exts
--                              (FieldExpr SelfExpr index0) -- riscv
--                              index1))
--                          ]
--                      }
