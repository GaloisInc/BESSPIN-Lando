{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lobot.Core.Kind.Examples
  ( -- * Example 1
    posint
  , IntPairType
  , unique_posint_pair
    -- * Example 2
  , ABCType
  , abc
  , abc_1
  , abc_2
  , abc_3
    -- * Example 3
  , SexType, PersonType
  , person
  , teenager
  , CoupleType
  , couple
  , straight_couple
  , teenage_couple
    -- * Example 4
  , RegWidthType, ExtsType, PrivType, VMType, RISCVType
  , riscv
  , SimType, BluespecBuildType
  , bluespec_build
    -- * Example 5
  , ColorType, AustraliaColoringType
  , australia_coloring
  ) where

import Data.List.NonEmpty
import Data.Parameterized.Classes
import Data.Parameterized.List
import Data.Parameterized.Some
import Lobot.Core.Kind

-- |
-- @
-- posint kind of int
--   where 0 <= self
-- @
posint :: Kind '[] IntType
posint = Kind { kindName = "posint"
              , kindType = knownRepr
              , kindFunctionEnv = Nil
              , kindConstraints =
                [ LteExpr (LiteralExpr (IntLit 0)) SelfExpr
                ]
              }

type IntPairType = StructType '[ '("x", IntType)
                               , '("y", IntType)
                               ]

-- |
-- @
-- unique_posint_pair kind of struct
--   with x : posint,
--        y : posint
--   where not (x = y),
--         x <= y
-- @
unique_posint_pair :: Kind '[] IntPairType
unique_posint_pair = liftConstraints index0 posint $
                     liftConstraints index1 posint $
                     Kind { kindName = "unique_posint_pair"
                          , kindType = knownRepr
                          , kindFunctionEnv = Nil
                          , kindConstraints =
                            [ NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index0)
                               (FieldExpr SelfExpr index1))
                            , LteExpr
                              (FieldExpr SelfExpr index0)
                              (FieldExpr SelfExpr index1)
                            ]
                          }

type ABCType = SetType '["A", "B", "C"]

-- |
-- @
-- abc kind of subset {A, B, C}
-- @
abc :: Kind '[] ABCType
abc = Kind { kindName = "abc"
           , kindType = knownRepr
           , kindFunctionEnv = Nil
           , kindConstraints = []
           }

-- |
-- @
-- abc_1 kind of abc where A in abc
-- @
abc_1 :: Kind '[] ABCType
abc_1 = derivedKind (abc :| []) "abc_1"
                  [ MemberExpr
                    (LiteralExpr (EnumLit knownRepr index0))
                    SelfExpr
                  ]

-- |
-- @
-- abc_2 kind of of abc where (A in abc) => (C in abc)
-- @
abc_2 :: Kind '[] ABCType
abc_2 = derivedKind (abc :| []) "abc_2"
             [ ImpliesExpr
               (MemberExpr
                (LiteralExpr (EnumLit knownRepr index0))
                SelfExpr)
               (MemberExpr
                (LiteralExpr (EnumLit knownRepr index2))
                SelfExpr)
             ]

-- |
-- @
-- abc2 kind of abc_1 abc_2
-- @
abc_3 :: Kind '[] ABCType
abc_3 = derivedKind
  (abc_1 :| [abc_2])
  "abc_3"
  []

type SexType = '["Male", "Female"]
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
person :: Kind '[] PersonType
person = Kind { kindName = "person"
              , kindType = knownRepr
              , kindFunctionEnv = Nil
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
teenager :: Kind '[] PersonType
teenager = derivedKind (person :| []) "teenager"
                [ LteExpr
                  (LiteralExpr (IntLit 13))
                  (FieldExpr SelfExpr index0)
                , LteExpr
                  (FieldExpr SelfExpr index0)
                  (LiteralExpr (IntLit 19))
                ]

type CoupleType = StructType '[ '("person1", PersonType)
                              , '("person2", PersonType)
                              ]

-- |
-- @
-- couple kind of struct
--   with person1 : person,
--        person2 : person
-- @
couple :: Kind '[] CoupleType
couple = Kind { kindName = "couple"
              , kindType = knownRepr
              , kindFunctionEnv = Nil
              , kindConstraints = []
              }

-- |
-- @
-- straight_couple kind of couple
--   where not (person1.sex = person2.sex)
-- @
straight_couple :: Kind '[] CoupleType
straight_couple = derivedKind (couple :| []) "straight_couple"
                       [ NotExpr
                         (EqExpr
                          (FieldExpr (FieldExpr SelfExpr index0) index1)
                          (FieldExpr (FieldExpr SelfExpr index1) index1))
                       ]

-- |
-- @ teenage_couple kind of couple
--     where person1 : teenager,
--           person2 : teenager
-- @
teenage_couple :: Kind '[] CoupleType
teenage_couple = liftConstraints index0 teenager $
                      liftConstraints index1 teenager $
                      derivedKind (couple :| []) "teenage_couple" []

type RegWidthType = '["RV32", "RV64"]

type ExtsType = '["M", "A", "F", "D", "C"]

type PrivType = '["PrivM", "PrivS", "PrivU"]

type VMType = '["SVNone", "SV32", "SV39", "SV48"]

type RISCVType = StructType '[ '("reg_width", EnumType RegWidthType),
                               '("xlen", IntType),
                               '("exts", SetType ExtsType),
                               '("privs", SetType PrivType),
                               '("vm", EnumType VMType)
                             ]

-- |
-- @
-- riscv kind of struct
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
riscv :: Kind '[] RISCVType
riscv = Kind { kindName = "riscv"
             , kindType = knownRepr
             , kindFunctionEnv = Nil
             , kindConstraints =
               [ ImpliesExpr
                 (EqExpr
                  (FieldExpr SelfExpr index0)
                  (LiteralExpr (EnumLit knownRepr index0)))
                 (EqExpr
                  (FieldExpr SelfExpr index1)
                  (LiteralExpr (IntLit 32)))
               , ImpliesExpr
                 (EqExpr
                  (FieldExpr SelfExpr index0)
                  (LiteralExpr (EnumLit knownRepr index1)))
                 (EqExpr
                  (FieldExpr SelfExpr index1)
                  (LiteralExpr (IntLit 64)))
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr index3))
                  (FieldExpr SelfExpr index2))
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr index2))
                  (FieldExpr SelfExpr index2))
               , MemberExpr
                 (LiteralExpr (EnumLit knownRepr index0))
                 (FieldExpr SelfExpr index3)
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr index1))
                  (FieldExpr SelfExpr index3))
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr index2))
                  (FieldExpr SelfExpr index3))
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr index1))
                  (FieldExpr SelfExpr index3))
                 (NotExpr
                  (EqExpr
                   (FieldExpr SelfExpr index4)
                   (LiteralExpr (EnumLit knownRepr index0))))
               , ImpliesExpr
                 (NotExpr
                  (MemberExpr
                   (LiteralExpr (EnumLit knownRepr index1))
                   (FieldExpr SelfExpr index3)))
                 (EqExpr
                  (FieldExpr SelfExpr index4)
                  (LiteralExpr (EnumLit knownRepr index0)))
               ]
             }

index4 :: Index (x0:x1:x2:x3:x4:r) x4
index4 = IndexThere index3

type SimType = EnumType '[ "Bluesim", "IVerilog", "Verilator" ]

type BluespecBuildType = StructType '[ '("riscv", RISCVType)
                                     , '("sim", SimType)
                                     , '("tv", BoolType)
                                     ]

-- |
-- @
-- bluespec_build kind of struct
--   with riscv : riscv,
--        sim : {Bluesim, IVerilog, Verilator}
--        tv : bool
--   where riscv.vm in {SVNone, SV32, SV39}
-- @
bluespec_build :: Kind '[] BluespecBuildType
bluespec_build = liftConstraints index0 riscv k
  where k = Kind { kindName = "bluespec_build"
                 , kindType = knownRepr
                 , kindFunctionEnv = Nil
                 , kindConstraints =
                   [ MemberExpr
                     (FieldExpr (FieldExpr SelfExpr index0) index4)
                     (LiteralExpr (SetLit knownRepr [ Some index0
                                                    , Some index1
                                                    , Some index2
                                                    ]))
                   ]
                 }

type ColorType = EnumType '["Red", "Blue", "Yellow"]
type AustraliaColoringType = StructType '[ '("WA", ColorType)
                                         , '("NT", ColorType)
                                         , '("SA", ColorType)
                                         , '("Q", ColorType)
                                         , '("NSW", ColorType)
                                         , '("V", ColorType)
                                         , '("T", ColorType)
                                         ]

-- |
-- This is a MiniZinc <https://www.minizinc.org/doc-2.4.3/en/modelling.html example>.
--
-- @
-- color is {Red, Blue, Yellow}
--
-- australia_coloring kind of struct
--   with WA : color,
--        NT : color,
--        SA : color,
--        Q : color,
--        NSW : color,
--        V : color,
--        T : color
--   where not (WA = NT),
--         not (WA = SA),
--         not (NT = SA),
--         not (NT = Q),
--         not (SA = Q),
--         not (SA = NSW),
--         not (SA = V),
--         not (Q = NSW),
--         not (NSW = V)
-- @
australia_coloring :: Kind '[] AustraliaColoringType
australia_coloring = Kind { kindName = "australia_coloring"
                          , kindType = knownRepr
                          , kindFunctionEnv = Nil
                          , kindConstraints =
                            [ NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index0)
                               (FieldExpr SelfExpr index1))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index0)
                               (FieldExpr SelfExpr index2))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index1)
                               (FieldExpr SelfExpr index2))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index1)
                               (FieldExpr SelfExpr index3))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index2)
                               (FieldExpr SelfExpr index3))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index2)
                               (FieldExpr SelfExpr index4))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index2)
                               (FieldExpr SelfExpr index5))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index3)
                               (FieldExpr SelfExpr index4))
                            , NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr index4)
                               (FieldExpr SelfExpr index5))
                            ]
                          }

index5 :: Index (x0:x1:x2:x3:x4:x5:r) x5
index5 = IndexThere index4
