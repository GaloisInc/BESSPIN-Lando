{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
  , InstanceResult(..)
  , getInstance
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

import Data.Foldable (forM_)
import Data.Parameterized.List
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Prelude hiding ((!!))

-- | Symbolic 'Literal'.
data SymLiteral t tp where
  BoolSym :: WB.BoolExpr t -> SymLiteral t BoolType
  IntSym  :: WB.IntegerExpr t -> SymLiteral t IntType
  -- | This implies a @bvPopCount == 1@ constraint.
  EnumSym :: 1 <= Length cs
          => List SymbolRepr cs
          -> WB.BVExpr t (Length cs)
          -> SymLiteral t (EnumType cs)
  SetSym  :: 1 <= Length cs
          => List SymbolRepr cs
          -> WB.BVExpr t (Length cs)
          -> SymLiteral t (SetType cs)
  KindSym :: SymInstance t ktps -> SymLiteral t (KindType ktps)

-- | Symbolic 'FieldLiteral'.
data SymFieldLiteral t (p :: (Symbol, Type)) where
  SymFieldLiteral :: { symFieldLiteralName :: SymbolRepr nm
                     , symFieldLiteralValue :: SymLiteral t tp
                     } -> SymFieldLiteral t '(nm, tp)

-- | Symbolic 'Instance'.
data SymInstance t ktps =
  SymInstance { symInstanceLiterals :: List (SymFieldLiteral t) ktps }

freshSymFieldConstant :: WS.SMTLib2Tweaks solver
                      => WB.ExprBuilder t st fs
                      -> String
                      -> FieldRepr '(nm, tp)
                      -> WS.Session t solver
                      -> IO (SymFieldLiteral t '(nm, tp))
freshSymFieldConstant sym prefix (FieldRepr nm tp) session = do
  let prefix' = prefix ++ T.unpack (symbolRepr nm)
      cSymbol = WI.safeSymbol prefix'
  case tp of
    BoolRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseBoolRepr
      return $ SymFieldLiteral nm (BoolSym c)
    IntRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseIntegerRepr
      return $ SymFieldLiteral nm (IntSym c)
    EnumRepr cs -> do
      -- We represent individual enumeration constants as 1 << n for some n.
      -- Therefore, we add a constraint that the popcount of the bitvector is 1.
      let n = ilength cs
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      c_popcount <- WI.bvPopcount sym c
      one <- WI.bvLit sym n (BV.one n)
      c_popcount_1 <- WI.bvEq sym c_popcount one
      WS.assume (WS.sessionWriter session) c_popcount_1
      return $ SymFieldLiteral nm (EnumSym cs c)
    SetRepr cs -> do
      let n = ilength cs
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      return $ SymFieldLiteral nm (SetSym cs c)
    KindRepr flds -> do
      c <- freshSymInstanceConstant sym prefix' flds session
      return $ SymFieldLiteral nm (KindSym c)

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

symLiteralEq :: WB.ExprBuilder t st fs
             -> SymLiteral t tp
             -> SymLiteral t tp
             -> IO (WB.BoolExpr t)
symLiteralEq sym (BoolSym b1) (BoolSym b2) = WI.eqPred sym b1 b2
symLiteralEq sym (IntSym x1) (IntSym x2) = WI.intEq sym x1 x2
symLiteralEq sym (EnumSym _ bv1) (EnumSym _ bv2) = WI.bvEq sym bv1 bv2
symLiteralEq sym (SetSym _ bv1) (SetSym _ bv2) = WI.bvEq sym bv1 bv2
symLiteralEq sym (KindSym i1) (KindSym i2) = symInstanceEq sym i1 i2

symFieldLiteralValueEq :: WB.ExprBuilder t st fs
                       -> SymFieldLiteral t '(nm, tp)
                       -> SymFieldLiteral t '(nm, tp)
                       -> IO (WB.BoolExpr t)
symFieldLiteralValueEq sym fv1 fv2 =
  symLiteralEq sym (symFieldLiteralValue fv1) (symFieldLiteralValue fv2)

symInstanceEq :: forall t st fs ktps .
                 WB.ExprBuilder t st fs
              -> SymInstance t ktps
              -> SymInstance t ktps
              -> IO (WB.BoolExpr t)
symInstanceEq sym inst1 inst2 =
  listEq (symInstanceLiterals inst1) (symInstanceLiterals inst2)
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

symInstance :: WB.ExprBuilder t st fs -> Instance ktps -> IO (SymInstance t ktps)
symInstance sym inst = do
  symFieldLiteralValues <- forFC (instanceValues inst) $ \fv@(FieldLiteral _ _) ->
    symFieldLiteral sym fv
  return $ SymInstance symFieldLiteralValues

symFieldLiteral :: WB.ExprBuilder t st fs
                -> FieldLiteral '(nm, tp)
                -> IO (SymFieldLiteral t '(nm, tp))
symFieldLiteral sym (FieldLiteral nm l) = SymFieldLiteral nm <$> symLiteral sym l

symLiteral :: WB.ExprBuilder t st fs -> Literal tp -> IO (SymLiteral t tp)
symLiteral sym l = case l of
  BoolLit True -> return $ BoolSym (WI.truePred sym)
  BoolLit False -> return $ BoolSym (WI.falsePred sym)
  IntLit x -> IntSym <$> WI.intLit sym x
  EnumLit cs i -> do
    let n = ilength cs
    EnumSym cs <$> WI.bvLit sym n (indexBit n (Some i))
  SetLit cs is -> do
    let n = ilength cs
    SetSym cs <$> WI.bvLit sym n (foldr BV.or (BV.zero n) (indexBit n <$> is))
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
    BoolSym <$> symLiteralEq sym sv1 sv2
  LteExpr e1 e2 -> do
    IntSym sv1 <- symEvalExpr sym inst e1
    IntSym sv2 <- symEvalExpr sym inst e2
    BoolSym <$> WI.intLe sym sv1 sv2
  MemberExpr e1 e2 -> do
    EnumSym _ elt_bv <- symEvalExpr sym inst e1
    SetSym _ set_bv <- symEvalExpr sym inst e2
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

natReprToIndex :: List f sh -> NatRepr n -> Maybe (Some (Index sh))
natReprToIndex Nil _ = Nothing
natReprToIndex (_ :< as) n = case isZeroOrGT1 n of
  Left Refl -> Just $ Some (IndexHere)
  Right LeqProof
    | Just (Some ix) <- natReprToIndex as (decNat n) -> Just (Some (IndexThere ix))
  _ -> Nothing

groundEvalFieldLiteral :: WG.GroundEvalFn t
                       -> SymFieldLiteral t ftp
                       -> IO (FieldLiteral ftp)
groundEvalFieldLiteral ge@WG.GroundEvalFn{..} SymFieldLiteral{..} = do
  l <- case symFieldLiteralValue of
         BoolSym sb -> BoolLit <$> groundEval sb
         IntSym sx -> IntLit <$> groundEval sx
         EnumSym cs sbv -> do
           let n = ilength cs
           bv <- groundEval sbv
           Some ixNat <- return $ mkNatRepr (BV.asNatural (BV.ctz n bv))
           Just (Some ix) <- return $ natReprToIndex cs ixNat
           return $ EnumLit cs ix
         SetSym cs sbv -> do
           let n = ilength cs
           bv <- groundEval sbv
           ixNats <- return $ map mkNatRepr
             [ i | i' <- [0..toInteger (natValue n) - 1]
                 , let i = fromInteger i'
                 , BV.testBit' i bv ]
           Just ixs <- return $ sequence (map (viewSome (natReprToIndex cs)) ixNats)
           return $ SetLit cs ixs
         KindSym sinst -> KindLit <$> groundEvalInstance ge sinst
  return $ FieldLiteral symFieldLiteralName l

-- | Evaluate a 'SymInstance' to an 'Instance' with the help of a ground
-- evaluation function (returned by a What4 solver).
groundEvalInstance :: WG.GroundEvalFn t
                   -> SymInstance t ktps
                   -> IO (Instance ktps)
groundEvalInstance ge SymInstance{..} = do
  fls <- traverseFC (groundEvalFieldLiteral ge) symInstanceLiterals
  return $ Instance fls

data BuilderState s = EmptyBuilderState

data InstanceResult ktps = HasInstance (Instance ktps)
                         | NoInstance
                         | Unknown
  deriving Show

-- | Given a 'Kind', get a satisfying 'Instance' if we can determine whether one
-- exists.
getInstance :: FilePath -> Kind ktps -> IO (InstanceResult ktps)
getInstance z3_path kd = do
  Some nonceGen <- newIONonceGenerator
  sym <- WB.newExprBuilder WB.FloatIEEERepr EmptyBuilderState nonceGen
  WC.extendConfig WS.z3Options (WI.getConfiguration sym)
  WS.withZ3 sym z3_path WS.defaultLogData $ \session -> do
    -- Create a fresh symbolic instance of our kind
    symInst <- freshSymInstanceConstant sym "" (kindFields kd) session
    -- Add all the kind constraints to our list of assumptions
    forM_ (kindConstraints kd) $ \e -> do
      BoolSym symConstraint <- symEvalExpr sym symInst e
      WS.assume (WS.sessionWriter session) symConstraint
    WS.runCheckSat session $ \result ->
      case result of
        WS.Sat (ge,_) -> do
          inst <- groundEvalInstance ge symInst
          return $ HasInstance inst
        WS.Unsat _ -> return NoInstance
        WS.Unknown -> return Unknown
