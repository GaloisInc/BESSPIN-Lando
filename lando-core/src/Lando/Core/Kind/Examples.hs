{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Lando.Core.Kind.Examples where

import Data.Parameterized.List
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
  , fieldType = EnumRepr (knownSymbol @"Male" :< knownSymbol @"Female" :< Nil)
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

male_person :: Kind PersonType
male_person = person `addConstraints`
  [ EqExpr
    (FieldExpr SelfExpr index1) -- sex
    (LiteralExpr (EnumLit index0)) -- male
  ]

female_person :: Kind PersonType
female_person = person `addConstraints`
  [ EqExpr
    (FieldExpr SelfExpr index1) -- sex
    (LiteralExpr (EnumLit index1)) -- female
  ]

child :: Kind PersonType
child = person `addConstraints`
  [ LteExpr (FieldExpr SelfExpr index0) (LiteralExpr (IntLit 18))]

ben :: Instance PersonType
ben = Instance { instanceValues =
                 FieldValue age (IntLit 30) :< FieldValue sex (EnumLit index0) :< Nil
               }

emmy :: Instance PersonType
emmy = Instance { instanceValues =
                  FieldValue age (IntLit 8) :< FieldValue sex (EnumLit index1) :< Nil
                }

archer :: Instance PersonType
archer = Instance { instanceValues =
                    FieldValue age (IntLit 4) :< FieldValue sex (EnumLit index0) :< Nil
                  }


reg_width :: FieldRepr '("reg_width", EnumType '["RV32", "RV64"])
reg_width = FieldRepr
  { fieldName = knownSymbol @"reg_width"
  , fieldType = EnumRepr (knownSymbol @"RV32" :< knownSymbol @"RV64" :< Nil)
  }

exts :: FieldRepr '("exts", SetType (EnumType '["M", "A", "F", "D", "C"]))
exts = FieldRepr
  { fieldName = knownSymbol @"exts"
  , fieldType = SetRepr $
    EnumRepr (knownSymbol @"M" :<
              knownSymbol @"A" :<
              knownSymbol @"F" :<
              knownSymbol @"D" :<
              knownSymbol @"C" :< Nil)
  }

type RISCVType = '[ '("reg_width", EnumType '["RV32", "RV64"]),
                    '("exts", SetType (EnumType '["M", "A", "F", "D", "C"]))
                  ]

riscv :: Kind RISCVType
riscv = Kind { kindName = "riscv"
             , kindFields = reg_width :< exts :< Nil
             , kindConstraints =
               [ ImpliesExpr
                 (MemberExpr -- D in exts
                  (LiteralExpr (EnumLit index3)) -- D
                  (FieldExpr SelfExpr index1)) -- exts
                 (MemberExpr -- F in exts
                  (LiteralExpr (EnumLit index2)) -- F
                  (FieldExpr SelfExpr index1)) -- exts
               ]
             }

riscv_with_m :: Kind RISCVType
riscv_with_m = riscv `addConstraints`
  [ MemberExpr
    (LiteralExpr (EnumLit index0)) -- M
    (FieldExpr SelfExpr index1) -- A
  ]

rv32i :: Instance RISCVType
rv32i = Instance { instanceValues =
                   FieldValue reg_width (EnumLit index0) :<
                   FieldValue exts (SetLit []) :<
                   Nil
                 }

rv32m :: Instance RISCVType
rv32m = Instance { instanceValues =
                   FieldValue reg_width (EnumLit index0) :<
                   FieldValue exts (SetLit [EnumLit index0]) :<
                   Nil
                 }

rv32d :: Instance RISCVType
rv32d = Instance { instanceValues =
                   FieldValue reg_width (EnumLit index0) :<
                   FieldValue exts (SetLit [EnumLit index3]) :<
                   Nil
                 }

rv64gc :: Instance RISCVType
rv64gc = Instance { instanceValues =
                    FieldValue reg_width (EnumLit index1) :<
                    FieldValue exts (SetLit [ EnumLit index0
                                            , EnumLit index1
                                            , EnumLit index2
                                            , EnumLit index3
                                            , EnumLit (IndexThere index3)
                                            ]) :<
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
                            (LiteralExpr (EnumLit index3)) -- riscv.D
                            (FieldExpr -- riscv.exts
                             (FieldExpr SelfExpr index0) -- riscv
                             index1))
                           (MemberExpr -- F in riscv.exts
                            (LiteralExpr (EnumLit index2)) -- riscv.F
                            (FieldExpr -- riscv.exts
                             (FieldExpr SelfExpr index0) -- riscv
                             index1))
                         ]
                     }
