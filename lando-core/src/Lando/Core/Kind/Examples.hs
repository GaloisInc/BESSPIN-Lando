{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Lando.Core.Kind.Examples where

import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Lando.Core.Kind

age :: FieldRepr '("age", IntType)
age = FieldRepr
  { fieldName = knownSymbol @"age"
  , fieldType = IntRepr
  }

sex :: FieldRepr '("sex", EnumType '["Male", "Female"])
sex = FieldRepr
  { fieldName = knownSymbol @"sex"
  , fieldType = EnumRepr (knownSymbol @"Male" :< knownSymbol @"Female" :< Nil) knownNat
  }

type PersonType = '[ '("age", IntType)
                   , '("sex", EnumType '["Male", "Female"])
                   ]

person :: Kind PersonType
person = Kind { kindName = "person"
              , kindFields = age :< sex :< Nil
              , kindConstraints =
                [ LteExpr (LiteralExpr (IntLit 0)) (FieldExpr SelfExpr index0) ]
              }

impossible_person :: Kind PersonType
impossible_person = person `addConstraints`
  [ EqExpr
    (FieldExpr SelfExpr index1) -- sex
    (LiteralExpr (EnumLit index0 knownNat)) -- male
  , EqExpr
    (FieldExpr SelfExpr index1) -- sex
    (LiteralExpr (EnumLit index1 knownNat)) -- female
  ]

male_person :: Kind PersonType
male_person = person `addConstraints`
  [ EqExpr
    (FieldExpr SelfExpr index1) -- sex
    (LiteralExpr (EnumLit index0 knownNat)) -- male
  ]

female_person :: Kind PersonType
female_person = person `addConstraints`
  [ EqExpr
    (FieldExpr SelfExpr index1) -- sex
    (LiteralExpr (EnumLit index1 knownNat)) -- female
  ]

child :: Kind PersonType
child = person `addConstraints`
  [ LteExpr (FieldExpr SelfExpr index0) (LiteralExpr (IntLit 18))]

ben :: Instance PersonType
ben = Instance { instanceValues =
                 FieldLiteral age (IntLit 30) :<
                 FieldLiteral sex (EnumLit index0 knownNat) :< Nil
               }

emmy :: Instance PersonType
emmy = Instance { instanceValues =
                  FieldLiteral age (IntLit 8) :<
                  FieldLiteral sex (EnumLit index1 knownNat) :< Nil
                }

archer :: Instance PersonType
archer = Instance { instanceValues =
                    FieldLiteral age (IntLit 4) :<
                    FieldLiteral sex (EnumLit index0 knownNat) :< Nil
                  }


reg_width :: FieldRepr '("reg_width", EnumType '["RV32", "RV64"])
reg_width = FieldRepr
  { fieldName = knownSymbol @"reg_width"
  , fieldType = EnumRepr (knownSymbol @"RV32" :< knownSymbol @"RV64" :< Nil) knownNat
  }

exts :: FieldRepr '("exts", SetType '["M", "A", "F", "D", "C"])
exts = FieldRepr
  { fieldName = knownSymbol @"exts"
  , fieldType = SetRepr (knownSymbol @"M" :<
                         knownSymbol @"A" :<
                         knownSymbol @"F" :<
                         knownSymbol @"D" :<
                         knownSymbol @"C" :< Nil)
                knownNat
  }

type RISCVType = '[ '("reg_width", EnumType '["RV32", "RV64"]),
                    '("exts", SetType '["M", "A", "F", "D", "C"])
                  ]

riscv :: Kind RISCVType
riscv = Kind { kindName = "riscv"
             , kindFields = reg_width :< exts :< Nil
             , kindConstraints =
               [ ImpliesExpr
                 (MemberExpr -- D in exts
                  (LiteralExpr (EnumLit index3 knownNat)) -- D
                  (FieldExpr SelfExpr index1)) -- exts
                 (MemberExpr -- F in exts
                  (LiteralExpr (EnumLit index2 knownNat)) -- F
                  (FieldExpr SelfExpr index1)) -- exts
               ]
             }

riscv_with_m :: Kind RISCVType
riscv_with_m = riscv `addConstraints`
  [ MemberExpr
    (LiteralExpr (EnumLit index0 knownNat)) -- M
    (FieldExpr SelfExpr index1) -- exts
  ]

riscv_with_only_d :: Kind RISCVType
riscv_with_only_d = riscv `addConstraints`
  [ EqExpr
    (FieldExpr SelfExpr index1) -- exts
    (LiteralExpr (SetLit [Some index3] knownNat)) -- D
  ]

rv32i :: Instance RISCVType
rv32i = Instance { instanceValues =
                   FieldLiteral reg_width (EnumLit index0 knownNat) :<
                   FieldLiteral exts (SetLit [] knownNat) :<
                   Nil
                 }

rv32m :: Instance RISCVType
rv32m = Instance { instanceValues =
                   FieldLiteral reg_width (EnumLit index0 knownNat) :<
                   FieldLiteral exts (SetLit [Some index0] knownNat) :<
                   Nil
                 }

rv32d :: Instance RISCVType
rv32d = Instance { instanceValues =
                   FieldLiteral reg_width (EnumLit index0 knownNat) :<
                   FieldLiteral exts (SetLit [Some index3] knownNat) :<
                   Nil
                 }

rv64gc :: Instance RISCVType
rv64gc = Instance { instanceValues =
                    FieldLiteral reg_width (EnumLit index1 knownNat) :<
                    FieldLiteral exts (SetLit [ Some index0
                                            , Some index1
                                            , Some index2
                                            , Some index3
                                            , Some (IndexThere index3)
                                            ] knownNat) :<
                    Nil
                  }

riscv_field :: FieldRepr '("riscv", KindType RISCVType)
riscv_field = FieldRepr { fieldName = knownSymbol @"riscv"
                        , fieldType = KindRepr (reg_width :< exts :< Nil)
                        }

sim :: FieldRepr '("sim", EnumType '["Bluesim", "IVerilog", "Verilator"])
sim = FieldRepr { fieldName = knownSymbol @"sim"
                , fieldType = EnumRepr (knownSymbol @"Bluesim" :<
                                        knownSymbol @"IVerilog" :<
                                        knownSymbol @"Verilator" :<
                                        Nil)
                              knownNat
                }

type PiccoloBuildType = '[ '("riscv", KindType RISCVType)
                         , '("sim", EnumType '["Bluesim", "IVerilog", "Verilator"])
                         ]

piccolo_build :: Kind PiccoloBuildType
piccolo_build = Kind { kindName = "piccolo_build"
                     , kindFields = riscv_field :< sim :< Nil
                     , kindConstraints =
                         [ ImpliesExpr -- inherited from riscv
                           (MemberExpr -- D in riscv.exts
                            (LiteralExpr (EnumLit index3 knownNat)) -- riscv.D
                            (FieldExpr -- riscv.exts
                             (FieldExpr SelfExpr index0) -- riscv
                             index1))
                           (MemberExpr -- F in riscv.exts
                            (LiteralExpr (EnumLit index2 knownNat)) -- riscv.F
                            (FieldExpr -- riscv.exts
                             (FieldExpr SelfExpr index0) -- riscv
                             index1))
                         ]
                     }
