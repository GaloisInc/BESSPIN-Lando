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
  --   -- * Example 5
  -- , ColorType, AustraliaColoringType
  -- , australia_coloring
  ) where

import Data.List.NonEmpty
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.Some
import Lobot.Core.Kind

-- |
-- @
-- posint kind of int
--   where 0 <= self
-- @
posint :: Kind EmptyCtx IntType
posint = Kind { kindName = "posint"
              , kindType = knownRepr
              , kindFunctionEnv = Empty
              , kindConstraints =
                [ LteExpr (LiteralExpr (IntLit 0)) SelfExpr
                ]
              }

type IntPairType = StructType (EmptyCtx ::> '("x", IntType) ::> '("y", IntType))

-- |
-- @
-- unique_posint_pair kind of struct
--   with x : posint,
--        y : posint
--   where not (x = y),
--         x <= y
-- @
unique_posint_pair :: Kind EmptyCtx IntPairType
unique_posint_pair = liftConstraints i1of2 posint $
                     liftConstraints i2of2 posint $
                     Kind { kindName = "unique_posint_pair"
                          , kindType = knownRepr
                          , kindFunctionEnv = Empty
                          , kindConstraints =
                            [ NotExpr
                              (EqExpr
                               (FieldExpr SelfExpr i1of2)
                               (FieldExpr SelfExpr i2of2))
                            , LteExpr
                              (FieldExpr SelfExpr i1of2)
                              (FieldExpr SelfExpr i2of2)
                            ]
                          }

type ABCType = SetType (EmptyCtx ::> "A" ::> "B" ::> "C")

-- |
-- @
-- abc kind of subset {A, B, C}
-- @
abc :: Kind EmptyCtx ABCType
abc = Kind { kindName = "abc"
           , kindType = knownRepr
           , kindFunctionEnv = Empty
           , kindConstraints = []
           }

-- |
-- @
-- abc_1 kind of abc where A in abc
-- @
abc_1 :: Kind EmptyCtx ABCType
abc_1 = derivedKind (abc :| []) "abc_1"
                  [ MemberExpr
                    (LiteralExpr (EnumLit knownRepr i1of3))
                    SelfExpr
                  ]

-- |
-- @
-- abc_2 kind of of abc where (A in abc) => (C in abc)
-- @
abc_2 :: Kind EmptyCtx ABCType
abc_2 = derivedKind (abc :| []) "abc_2"
             [ ImpliesExpr
               (MemberExpr
                (LiteralExpr (EnumLit knownRepr i1of3))
                SelfExpr)
               (MemberExpr
                (LiteralExpr (EnumLit knownRepr i3of3))
                SelfExpr)
             ]

-- |
-- @
-- abc2 kind of abc_1 abc_2
-- @
abc_3 :: Kind EmptyCtx ABCType
abc_3 = derivedKind
  (abc_1 :| [abc_2])
  "abc_3"
  []

type SexType = EmptyCtx ::> "Male" ::> "Female"
type PersonType = StructType (EmptyCtx
                               ::> '("age", IntType)
                               ::> '("sex", EnumType SexType))

-- |
-- @
-- person kind of struct
--   with age : int
--        sex : {Male, Female}
--   where age >= 0
-- @
person :: Kind EmptyCtx PersonType
person = Kind { kindName = "person"
              , kindType = knownRepr
              , kindFunctionEnv = Empty
              , kindConstraints =
                  [ LteExpr
                    (LiteralExpr (IntLit 0))
                    (FieldExpr SelfExpr i1of2)
                  ]
              }

-- |
-- @
-- teenager kind of person
--   where 13 <= age,
--         age <= 19
-- @
teenager :: Kind EmptyCtx PersonType
teenager = derivedKind (person :| []) "teenager"
                [ LteExpr
                  (LiteralExpr (IntLit 13))
                  (FieldExpr SelfExpr i1of2)
                , LteExpr
                  (FieldExpr SelfExpr i1of2)
                  (LiteralExpr (IntLit 19))
                ]

type CoupleType = StructType (EmptyCtx
                              ::> '("person1", PersonType)
                              ::> '("person2", PersonType))

-- |
-- @
-- couple kind of struct
--   with person1 : person,
--        person2 : person
-- @
couple :: Kind EmptyCtx CoupleType
couple = Kind { kindName = "couple"
              , kindType = knownRepr
              , kindFunctionEnv = Empty
              , kindConstraints = []
              }

-- |
-- @
-- straight_couple kind of couple
--   where not (person1.sex = person2.sex)
-- @
straight_couple :: Kind EmptyCtx CoupleType
straight_couple = derivedKind (couple :| []) "straight_couple"
                       [ NotExpr
                         (EqExpr
                          (FieldExpr (FieldExpr SelfExpr i1of2) i2of2)
                          (FieldExpr (FieldExpr SelfExpr i2of2) i2of2))
                       ]

-- |
-- @ teenage_couple kind of couple
--     where person1 : teenager,
--           person2 : teenager
-- @
teenage_couple :: Kind EmptyCtx CoupleType
teenage_couple = liftConstraints i1of2 teenager $
                 liftConstraints i2of2 teenager $
                 derivedKind (couple :| []) "teenage_couple" []

type RegWidthType = EmptyCtx ::> "RV32" ::> "RV64"

type ExtsType = EmptyCtx ::> "M" ::> "A" ::> "F" ::> "D" ::> "C"

type PrivType = EmptyCtx ::> "PrivM" ::>  "PrivS" ::> "PrivU"

type VMType = EmptyCtx ::> "SVNone" ::> "SV32" ::> "SV39" ::> "SV48"

type RISCVType = StructType (EmptyCtx
                             ::> '("reg_width", EnumType RegWidthType)
                             ::> '("xlen", IntType)
                             ::> '("exts", SetType ExtsType)
                             ::> '("privs", SetType PrivType)
                             ::> '("vm", EnumType VMType))

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
riscv :: Kind EmptyCtx RISCVType
riscv = Kind { kindName = "riscv"
             , kindType = knownRepr
             , kindFunctionEnv = Empty
             , kindConstraints =
               [ ImpliesExpr
                 (EqExpr
                  (FieldExpr SelfExpr i1of5)
                  (LiteralExpr (EnumLit knownRepr i1of2)))
                 (EqExpr
                  (FieldExpr SelfExpr i2of5)
                  (LiteralExpr (IntLit 32)))
               , ImpliesExpr
                 (EqExpr
                  (FieldExpr SelfExpr i1of5)
                  (LiteralExpr (EnumLit knownRepr i2of2)))
                 (EqExpr
                  (FieldExpr SelfExpr i2of5)
                  (LiteralExpr (IntLit 64)))
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr i4of5))
                  (FieldExpr SelfExpr i3of5))
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr i3of5))
                  (FieldExpr SelfExpr i3of5))
               , MemberExpr
                 (LiteralExpr (EnumLit knownRepr i1of3))
                 (FieldExpr SelfExpr i4of5)
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr i2of3))
                  (FieldExpr SelfExpr i4of5))
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr i3of3))
                  (FieldExpr SelfExpr i4of5))
               , ImpliesExpr
                 (MemberExpr
                  (LiteralExpr (EnumLit knownRepr i2of3))
                  (FieldExpr SelfExpr i4of5))
                 (NotExpr
                  (EqExpr
                   (FieldExpr SelfExpr i5of5)
                   (LiteralExpr (EnumLit knownRepr i1of4))))
               , ImpliesExpr
                 (NotExpr
                  (MemberExpr
                   (LiteralExpr (EnumLit knownRepr i2of3))
                   (FieldExpr SelfExpr i4of5)))
                 (EqExpr
                  (FieldExpr SelfExpr i5of5)
                  (LiteralExpr (EnumLit knownRepr i1of4)))
               ]
             }

type SimType = EnumType (EmptyCtx
                         ::> "Bluesim"
                         ::> "IVerilog"
                         ::> "Verilator")

type BluespecBuildType = StructType (EmptyCtx
                                     ::> '("riscv", RISCVType)
                                     ::> '("sim", SimType)
                                     ::> '("tv", BoolType))

-- |
-- @
-- bluespec_build kind of struct
--   with riscv : riscv,
--        sim : {Bluesim, IVerilog, Verilator}
--        tv : bool
--   where riscv.vm in {SVNone, SV32, SV39}
-- @
bluespec_build :: Kind EmptyCtx BluespecBuildType
bluespec_build = liftConstraints i1of3 riscv k
  where k = Kind { kindName = "bluespec_build"
                 , kindType = knownRepr
                 , kindFunctionEnv = Empty
                 , kindConstraints =
                   [ MemberExpr
                     (FieldExpr (FieldExpr SelfExpr i1of3) i5of5)
                     (LiteralExpr (SetLit knownRepr [ Some i1of4
                                                    , Some i2of4
                                                    , Some i3of4
                                                    ]))
                   ]
                 }

-- type ColorType = EnumType (EmptyCtx
--                            ::> "Red"
--                            ::> "Blue"
--                            ::> "Yellow")
-- type AustraliaColoringType = StructType (EmptyCtx
--                                          ::> '("WA", ColorType)
--                                          ::> '("NT", ColorType)
--                                          ::> '("SA", ColorType)
--                                          ::> '("Q", ColorType)
--                                          ::> '("NSW", ColorType)
--                                          ::> '("V", ColorType)
--                                          ::> '("T", ColorType))

-- -- |
-- -- This is a MiniZinc <https://www.minizinc.org/doc-2.4.3/en/modelling.html example>.
-- --
-- -- @
-- -- color is {Red, Blue, Yellow}
-- --
-- -- australia_coloring kind of struct
-- --   with WA : color,
-- --        NT : color,
-- --        SA : color,
-- --        Q : color,
-- --        NSW : color,
-- --        V : color,
-- --        T : color
-- --   where not (WA = NT),
-- --         not (WA = SA),
-- --         not (NT = SA),
-- --         not (NT = Q),
-- --         not (SA = Q),
-- --         not (SA = NSW),
-- --         not (SA = V),
-- --         not (Q = NSW),
-- --         not (NSW = V)
-- -- @
-- australia_coloring :: Kind EmptyCtx AustraliaColoringType
-- australia_coloring = Kind { kindName = "australia_coloring"
--                           , kindType = knownRepr
--                           , kindFunctionEnv = Empty
--                           , kindConstraints =
--                             [ NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index0)
--                                (FieldExpr SelfExpr index1))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index0)
--                                (FieldExpr SelfExpr index2))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index1)
--                                (FieldExpr SelfExpr index2))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index1)
--                                (FieldExpr SelfExpr index3))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index2)
--                                (FieldExpr SelfExpr index3))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index2)
--                                (FieldExpr SelfExpr index4))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index2)
--                                (FieldExpr SelfExpr index5))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index3)
--                                (FieldExpr SelfExpr index4))
--                             , NotExpr
--                               (EqExpr
--                                (FieldExpr SelfExpr index4)
--                                (FieldExpr SelfExpr index5))
--                             ]
--                           }
