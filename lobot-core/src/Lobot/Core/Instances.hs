{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Lobot.Core.Instances
Description : Enumerating instances via what4/Z3.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module provides functions to enumerate instances of a kind via a
what4-based SMT solver backend.
-}
module Lobot.Core.Instances
  ( SymLiteral(..)
  , SymFieldLiteral(..)
  , symEvalExpr
  , InstanceResult(..)
  , getInstance
  , getNextInstance
  , countInstances
  , instanceSession
  ) where

import Data.Parameterized.List.Length
import Lobot.Core.Kind
import Lobot.Core.Kind.Pretty

import qualified Data.BitVector.Sized    as BV
import qualified Data.Parameterized.List as PL
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
import Data.IORef
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
  StructSym :: List (SymFieldLiteral t) ktps -> SymLiteral t (StructType ktps)

-- | Symbolic 'FieldLiteral'.
data SymFieldLiteral t (p :: (Symbol, Type)) where
  SymFieldLiteral :: { symFieldLiteralName :: SymbolRepr nm
                     , symFieldLiteralValue :: SymLiteral t tp
                     } -> SymFieldLiteral t '(nm, tp)

freshSymFieldLiteralConstant :: WS.SMTLib2Tweaks solver
                             => WB.ExprBuilder t st fs
                             -> String
                             -> FieldRepr ftp
                             -> WS.Session t solver
                             -> IO (SymFieldLiteral t ftp)
freshSymFieldLiteralConstant sym prefix (FieldRepr nm tp) session =
  SymFieldLiteral nm <$> freshSymLiteralConstant sym prefix tp session

-- | Symbolic 'Literal'.
freshSymLiteralConstant :: WS.SMTLib2Tweaks solver
                        => WB.ExprBuilder t st fs
                        -> String
                        -> TypeRepr tp
                        -> WS.Session t solver
                        -> IO (SymLiteral t tp)
freshSymLiteralConstant sym prefix tp session = do
  let cSymbol = WI.safeSymbol prefix
  case tp of
    BoolRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseBoolRepr
      return $ BoolSym c
    IntRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseIntegerRepr
      return $ IntSym c
    EnumRepr cs -> do
      -- We represent individual enumeration constants as 1 << n for some n.
      -- Therefore, we add a constraint that the popcount of the bitvector is 1.
      let n = ilength cs
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      c_popcount <- WI.bvPopcount sym c
      one <- WI.bvLit sym n (BV.one n)
      c_popcount_1 <- WI.bvEq sym c_popcount one
      WS.assume (WS.sessionWriter session) c_popcount_1
      return $ EnumSym cs c
    SetRepr cs -> do
      let n = ilength cs
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      return $ SetSym cs c
    StructRepr flds -> do
      cs <- forFC flds $ \fld -> freshSymFieldLiteralConstant sym prefix fld session
      return $ StructSym cs

symLiteralEq :: forall t st fs tp .
                WB.ExprBuilder t st fs
             -> SymLiteral t tp
             -> SymLiteral t tp
             -> IO (WB.BoolExpr t)
symLiteralEq sym (BoolSym b1) (BoolSym b2) = WI.eqPred sym b1 b2
symLiteralEq sym (IntSym x1) (IntSym x2) = WI.intEq sym x1 x2
symLiteralEq sym (EnumSym _ bv1) (EnumSym _ bv2) = WI.bvEq sym bv1 bv2
symLiteralEq sym (SetSym _ bv1) (SetSym _ bv2) = WI.bvEq sym bv1 bv2
symLiteralEq sym (StructSym sfls1) (StructSym sfls2) = sflsEq sfls1 sfls2
  where sflsEq :: forall (sh :: [(Symbol, Type)]) .
                  List (SymFieldLiteral t) sh
               -> List (SymFieldLiteral t) sh
               -> IO (WB.BoolExpr t)
        sflsEq Nil Nil = return $ WI.truePred sym
        sflsEq (a :< as) (b :< bs)
          | SymFieldLiteral _ _ <- a = do
              abEq <- symFieldLiteralValueEq sym a b
              rstEq <- sflsEq as bs
              WI.andPred sym abEq rstEq

symFieldLiteralValueEq :: WB.ExprBuilder t st fs
                       -> SymFieldLiteral t '(nm, tp)
                       -> SymFieldLiteral t '(nm, tp)
                       -> IO (WB.BoolExpr t)
symFieldLiteralValueEq sym fv1 fv2 =
  symLiteralEq sym (symFieldLiteralValue fv1) (symFieldLiteralValue fv2)

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
  StructLit fls -> do
    symFieldLiteralValues <- forFC fls $ \fl@(FieldLiteral _ _) ->
      symFieldLiteral sym fl
    return $ StructSym symFieldLiteralValues
  where indexBit n (Some i) = BV.bit' n (fromInteger (PL.indexValue i))

-- | Symbolically evaluate an expression given a symbolic instance.
symEvalExpr :: WB.ExprBuilder t st fs
            -> SymLiteral t tp
            -> Expr env tp tp'
            -> IO (SymLiteral t tp')
symEvalExpr sym symLit e = case e of
  LiteralExpr l -> symLiteral sym l
  SelfExpr -> return symLit
  FieldExpr kd fld -> do
    StructSym sfls <- symEvalExpr sym symLit kd
    return $ symFieldLiteralValue (sfls !! fld)
  EqExpr e1 e2 -> do
    sv1 <- symEvalExpr sym symLit e1
    sv2 <- symEvalExpr sym symLit e2
    BoolSym <$> symLiteralEq sym sv1 sv2
  LteExpr e1 e2 -> do
    IntSym sv1 <- symEvalExpr sym symLit e1
    IntSym sv2 <- symEvalExpr sym symLit e2
    BoolSym <$> WI.intLe sym sv1 sv2
  MemberExpr e1 e2 -> do
    EnumSym _ elt_bv <- symEvalExpr sym symLit e1
    SetSym _ set_bv <- symEvalExpr sym symLit e2
    elt_bv_and_set_bv <- WI.bvAndBits sym elt_bv set_bv
    elt_bv_in_set_bv <- WI.bvEq sym elt_bv elt_bv_and_set_bv
    return $ BoolSym elt_bv_in_set_bv
  ImpliesExpr e1 e2 -> do
    BoolSym b1 <- symEvalExpr sym symLit e1
    BoolSym b2 <- symEvalExpr sym symLit e2
    BoolSym <$> WI.impliesPred sym b1 b2
  NotExpr e' -> do
    BoolSym b <- symEvalExpr sym symLit e'
    BoolSym <$> WI.notPred sym b

groundEvalFieldLiteral :: WG.GroundEvalFn t
                       -> SymFieldLiteral t ftp
                       -> IO (FieldLiteral ftp)
groundEvalFieldLiteral ge SymFieldLiteral{..} =
  FieldLiteral symFieldLiteralName <$> groundEvalLiteral ge symFieldLiteralValue

groundEvalLiteral :: WG.GroundEvalFn t
                  -> SymLiteral t tp
                  -> IO (Literal tp)
groundEvalLiteral ge@WG.GroundEvalFn{..} symLit = case symLit of
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
    Just ixs <- return $
      sequence (map (viewSome (natReprToIndex cs))
                ixNats)
    return $ SetLit cs ixs
  StructSym sfls -> do
    fls <- traverseFC (groundEvalFieldLiteral ge) sfls
    return $ StructLit fls

data BuilderState s = EmptyBuilderState

data InstanceResult tp = HasInstance (Literal tp)
                       | NoInstance
                       | Unknown
  deriving Show

-- TODO: Rewrite using 'withSession'
-- | Given a 'Kind', get a satisfying 'Instance' if we can determine whether one
-- exists.
getInstance :: FilePath -> Kind env ktps -> IO (InstanceResult ktps)
getInstance z3_path kd = do
  Some nonceGen <- newIONonceGenerator
  sym <- WB.newExprBuilder WB.FloatIEEERepr EmptyBuilderState nonceGen
  WC.extendConfig WS.z3Options (WI.getConfiguration sym)
  WS.withZ3 sym z3_path WS.defaultLogData $ \session -> do
    -- Create a fresh symbolic instance of our kind
    symLit <- freshSymLiteralConstant sym "" (kindType kd) session
    -- Add all the kind constraints to our list of assumptions
    forM_ (kindConstraints kd) $ \e -> do
      BoolSym symConstraint <- symEvalExpr sym symLit e
      WS.assume (WS.sessionWriter session) symConstraint
    WS.runCheckSat session $ \result ->
      case result of
        WS.Sat (ge,_) -> do
          inst <- groundEvalLiteral ge symLit
          return $ HasInstance inst
        WS.Unsat _ -> return NoInstance
        WS.Unknown -> return Unknown

-- | If there are any instances in the current session, retrieve it, and then
-- negate that instance so we get a different result next time.
getNextInstance :: WS.SMTLib2Tweaks solver
                => WB.ExprBuilder t st fs
                -> WS.Session t solver
                -> SymLiteral t tp
                -> IO (InstanceResult tp)
getNextInstance sym session symLit = WS.runCheckSat session $ \result ->
  case result of
    WS.Sat (ge,_) -> do
      inst <- groundEvalLiteral ge symLit
      let negateExpr = NotExpr (EqExpr SelfExpr (LiteralExpr inst))
      BoolSym symConstraint <- symEvalExpr sym symLit negateExpr
      WS.assume (WS.sessionWriter session) symConstraint
      return $ HasInstance inst
    WS.Unsat _ -> do return NoInstance
    WS.Unknown -> do return Unknown

repeatIO :: Show a => (String -> IO (Maybe a)) -> IO ()
repeatIO k = do
  s <- getLine
  ma <- k s
  case ma of
    Just a -> do print a
                 repeatIO k
    Nothing -> return ()

-- | Run an interactive session for instance generation. Every time the user
-- hits \"Enter\", a new instance is provided.
instanceSession :: FilePath -> Kind env ktps -> IO ()
instanceSession = withSession $ \sym session symInst -> do
  i <- newIORef (0 :: Integer)
  WS.runCheckSat session $ \_ -> repeatIO $ \_ -> do
    iVal <- readIORef i
    let iVal' = iVal + 1
    writeIORef i iVal'
    res <- getNextInstance sym session symInst
    case res of
      HasInstance inst -> do
        putStrLn $ "Instance #" ++ show iVal' ++ ":"
        return $ Just (ppLiteral inst)
      _ -> return Nothing

countInstances' :: Integer -> SolverSessionFn ktps Integer
countInstances' 0 _ _ _ = return 0
countInstances' limit sym session symLit = do
  nextInstance <- getNextInstance sym session symLit
  case nextInstance of
    HasInstance _ -> do n <- countInstances' (limit-1) sym session symLit
                        return $ n + 1
    _ -> return 0

-- | Count the total number of instances, up to a certain limit.
countInstances :: Integer -- ^ Maximum number to count to
               -> FilePath
               -> Kind env ktps
               -> IO Integer
countInstances limit = withSession (countInstances' limit)

type SolverSessionFn tp a = forall t . WB.ExprBuilder t BuilderState (WB.Flags WB.FloatIEEE) -> WS.Session t WS.Z3 -> SymLiteral t tp -> IO a

withSession :: SolverSessionFn tp a
            -> FilePath
            -> Kind env tp
            -> IO a
withSession k z3_path kd = do
  Some nonceGen <- newIONonceGenerator
  sym <- WB.newExprBuilder WB.FloatIEEERepr EmptyBuilderState nonceGen
  WC.extendConfig WS.z3Options (WI.getConfiguration sym)
  WS.withZ3 sym z3_path WS.defaultLogData $ \session -> do
    symLit <- freshSymLiteralConstant sym "" (kindType kd) session
    forM_ (kindConstraints kd) $ \e -> do
      BoolSym symConstraint <- symEvalExpr sym symLit e
      WS.assume (WS.sessionWriter session) symConstraint
    k sym session symLit
