{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lando.Core.Instances
  ( SymInstance(..)
  , SymFieldLiteral(..)
  , SymLiteral(..)
  , symEvalExpr
  ) where

import Data.Parameterized.List.Length
import Lando.Core.Kind

import qualified Data.BitVector.Sized    as BV
import qualified Data.Parameterized.List as PL
import qualified Data.Text               as T
import qualified What4.Expr.Builder      as WB
import qualified What4.Config            as WC
import qualified What4.Expr.GroundEval   as WG
import qualified What4.Interface         as WI
import qualified What4.Protocol.SMTLib2  as WS
import qualified What4.SatResult         as WS
import qualified What4.Solver            as WS
import qualified What4.Solver.Z3         as WS
import qualified What4.BaseTypes         as WT

import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import GHC.TypeLits
import Prelude hiding ((!!))

-- | Symbolic 'Literal'.
data SymLiteral t tp where
  BoolSym :: WB.BoolExpr t -> SymLiteral t BoolType
  IntSym  :: WB.IntegerExpr t -> SymLiteral t IntType
  -- | This needs a @bvPopCount == 1@ constraint
  EnumSym :: 1 <= Length cs => WB.BVExpr t (Length cs) -> SymLiteral t (EnumType cs)
  SetSym  :: 1 <= Length cs => WB.BVExpr t (Length cs) -> SymLiteral t (SetType cs)
  KindSym :: SymInstance t ktps -> SymLiteral t (KindType ktps)

-- | Symbolic 'FieldLiteral'.
data SymFieldLiteral t (p :: (Symbol, Type)) where
  SymFieldLiteral :: { symFieldLiteralType :: FieldRepr '(nm, tp)
                     , symFieldLiteralValue :: SymLiteral t tp
                     } -> SymFieldLiteral t '(nm, tp)

-- | Symbolic 'Instance'.
data SymInstance t ktps =
  SymInstance { symInstanceValues :: List (SymFieldLiteral t) ktps }

freshSymFieldConstant :: WS.SMTLib2Tweaks solver
                      => WB.ExprBuilder t st fs
                      -> String
                      -> FieldRepr '(nm, tp)
                      -> WS.Session t solver
                      -> IO (SymFieldLiteral t '(nm, tp))
freshSymFieldConstant sym prefix ftp@(FieldRepr nm tp) session = do
  let prefix' = prefix ++ T.unpack (symbolRepr nm)
      cSymbol = WI.safeSymbol prefix'
  case tp of
    BoolRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseBoolRepr
      return $ SymFieldLiteral ftp (BoolSym c)
    IntRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseIntegerRepr
      return $ SymFieldLiteral ftp (IntSym c)
    EnumRepr _ n -> do
      -- We represent individual enumeration constants as 1 << n for some n.
      -- Therefore, we add a constraint that the popcount of the bitvector is 1.
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      c_popcount <- WI.bvPopcount sym c
      one <- WI.bvLit sym n (BV.one n)
      c_popcount_1 <- WI.bvEq sym c_popcount one
      WS.assume (WS.sessionWriter session) c_popcount_1
      return $ SymFieldLiteral ftp (EnumSym c)
    SetRepr _ n -> do
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      return $ SymFieldLiteral ftp (SetSym c)
    KindRepr flds -> do
      c <- freshSymInstanceConstant sym prefix' flds session
      return $ SymFieldLiteral ftp (KindSym c)

freshSymInstanceConstant :: WS.SMTLib2Tweaks solver
                         => WB.ExprBuilder t st fs
                         -> String
                         -> List FieldRepr ktps
                         -> WS.Session t solver
                         -> IO (SymInstance t ktps)
freshSymInstanceConstant sym prefix flds session = do
  cs <- forFC flds $ \ftp@(FieldRepr nm _) -> do
    let prefix' = prefix ++ T.unpack (symbolRepr nm)
    freshSymFieldConstant sym prefix' ftp session
  return $ SymInstance cs

symValueEq :: WB.ExprBuilder t st fs
           -> SymLiteral t tp
           -> SymLiteral t tp
           -> IO (WB.BoolExpr t)
symValueEq sym (BoolSym b1) (BoolSym b2) = WI.eqPred sym b1 b2
symValueEq sym (IntSym x1) (IntSym x2) = WI.intEq sym x1 x2
symValueEq sym (EnumSym bv1) (EnumSym bv2) = WI.bvEq sym bv1 bv2
symValueEq sym (SetSym bv1) (SetSym bv2) = WI.bvEq sym bv1 bv2
symValueEq sym (KindSym i1) (KindSym i2) = symInstanceEq sym i1 i2

symFieldLiteralValueEq :: WB.ExprBuilder t st fs
                -> SymFieldLiteral t '(nm, tp)
                -> SymFieldLiteral t '(nm, tp)
                -> IO (WB.BoolExpr t)
symFieldLiteralValueEq sym fv1 fv2 = symValueEq sym (symFieldLiteralValue fv1) (symFieldLiteralValue fv2)

symInstanceEq :: forall t st fs ktps .
                 WB.ExprBuilder t st fs
              -> SymInstance t ktps
              -> SymInstance t ktps
              -> IO (WB.BoolExpr t)
symInstanceEq sym inst1 inst2 =
  listEq (symInstanceValues inst1) (symInstanceValues inst2)
  where listEq :: forall (sh :: [(Symbol, Type)]) .
                  List (SymFieldLiteral t) sh
               -> List (SymFieldLiteral t) sh
               -> IO (WB.BoolExpr t)
        listEq Nil Nil = return $ WI.truePred sym
        listEq (a :< as) (b :< bs)
          | SymFieldLiteral _ _ <- a = do
              abEq <- symFieldLiteralValueEq sym a b
              rstEq <- listEq as bs
              WI.andPred sym abEq rstEq

symFieldValue :: WB.ExprBuilder t st fs
              -> FieldLiteral '(nm, tp)
              -> IO (SymFieldLiteral t '(nm, tp))
symFieldValue sym (FieldLiteral tp l) = SymFieldLiteral tp <$> symLiteral sym l

symInstance :: WB.ExprBuilder t st fs -> Instance ktps -> IO (SymInstance t ktps)
symInstance sym inst = do
  symFieldLiteralValues <- forFC (instanceValues inst) $ \fv@(FieldLiteral _ _) ->
    symFieldValue sym fv
  return $ SymInstance symFieldLiteralValues

symLiteral :: WB.ExprBuilder t st fs -> Literal tp -> IO (SymLiteral t tp)
symLiteral sym l = case l of
  BoolLit True -> return $ BoolSym (WI.truePred sym)
  BoolLit False -> return $ BoolSym (WI.falsePred sym)
  IntLit x -> IntSym <$> WI.intLit sym x
  EnumLit i n -> EnumSym <$> WI.bvLit sym n (indexBit n (Some i))
  SetLit is n -> SetSym <$> WI.bvLit sym n (foldr BV.or (BV.zero n) (indexBit n <$> is))
  KindLit inst -> KindSym <$> symInstance sym inst
  where indexBit n (Some i) = BV.bit' n (fromInteger (PL.indexValue i))

-- | Symbolically evaluate an expression given a symbolic instance.
symEvalExpr :: WB.ExprBuilder t st fs
            -> SymInstance t ktps
            -> Expr ktps tp
            -> IO (SymLiteral t tp)
symEvalExpr sym inst e = case e of
  LiteralExpr l -> symLiteral sym l
  SelfExpr -> return $ KindSym inst
  FieldExpr kd fld -> do
    KindSym (SymInstance sfvs) <- symEvalExpr sym inst kd
    return $ symFieldLiteralValue (sfvs !! fld)
  EqExpr e1 e2 -> do
    sv1 <- symEvalExpr sym inst e1
    sv2 <- symEvalExpr sym inst e2
    BoolSym <$> symValueEq sym sv1 sv2
  LteExpr e1 e2 -> do
    IntSym sv1 <- symEvalExpr sym inst e1
    IntSym sv2 <- symEvalExpr sym inst e2
    BoolSym <$> WI.intLe sym sv1 sv2
  MemberExpr e1 e2 -> do
    EnumSym elt_bv <- symEvalExpr sym inst e1
    SetSym set_bv <- symEvalExpr sym inst e2
    elt_bv_and_set_bv <- WI.bvAndBits sym elt_bv set_bv
    elt_bv_in_set_bv <- WI.bvEq sym elt_bv elt_bv_and_set_bv
    return $ BoolSym elt_bv_in_set_bv
  ImpliesExpr e1 e2 -> do
    BoolSym b1 <- symEvalExpr sym inst e1
    BoolSym b2 <- symEvalExpr sym inst e2
    BoolSym <$> WI.impliesPred sym b1 b2
  NotExpr e' -> do
    BoolSym b <- symEvalExpr sym inst e'
    BoolSym <$> WI.notPred sym b

-- data BuilderState s = BuilderState

-- data Result = Sat (BV.BV 5) | Unsat | Unknown
--   deriving Show

-- what4Example :: IO Result
-- what4Example = do
--   Some nonceGen <- newIONonceGenerator
--   sym <- WB.newExprBuilder WB.FloatIEEERepr BuilderState nonceGen
--   x <- WI.freshConstant sym (WI.safeSymbol "x") (WT.BaseBVRepr (knownNat @5))
--   x_1 <- WI.testBitBV sym 1 x
--   x_3 <- WI.testBitBV sym 3 x
--   x_1_implies_x_3 <- WI.impliesPred sym x_1 x_3
--   a <- WI.andPred sym x_1 x_1_implies_x_3
--   ten <- WI.bvLit sym (knownNat @5) (BV.mkBV knownNat 10)
--   x_eq_10 <- WI.bvEq sym x ten
--   x_neq_10 <- WI.notPred sym x_eq_10
--   b <- WI.andPred sym a x_neq_10
--   WC.extendConfig WS.z3Options (WI.getConfiguration sym)
--   WS.withZ3 sym "/usr/local/bin/z3" WS.defaultLogData $ \ session -> do
--     WS.assume (WS.sessionWriter session) b
--     WS.runCheckSat session $ \result ->
--       case result of
--         WS.Sat (geval, _) -> Sat <$> WG.groundEval geval x
--         WS.Unsat _ -> return Unsat
--         WS.Unknown -> return Unknown
