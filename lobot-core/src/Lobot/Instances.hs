{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Lobot.Instances
Description : Enumerating instances via what4/Z3.
Copyright   : (c) Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module provides functions to enumerate instances of a kind via a
what4-based SMT solver backend.
-}
module Lobot.Instances
  ( SessionData(..)
  , runSession
  , InstanceResult(..)
  , pattern ValidInstance
  , pattern InvalidInstance
  , getNextInstance
  , collectAndFilterInstances
  ) where

import Lobot.Expr
import Lobot.Kind
import Lobot.Types
import Lobot.Utils

import qualified Data.BitVector.Sized    as BV
import qualified Data.Text               as T
import qualified What4.Expr.Builder      as WB
import qualified What4.Config            as WC
import qualified What4.Expr.GroundEval   as WG
import qualified What4.Interface         as WI
import qualified What4.Protocol.SMTLib2  as WS
import qualified What4.SatResult         as WS
import qualified What4.Solver            as WS
import qualified What4.BaseTypes         as WT

import Data.Foldable (forM_, traverse_)
import Data.Parameterized.BoolRepr
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Numeric.Natural
import Prelude hiding ((!!))
import Unsafe.Coerce (unsafeCoerce) -- I know, I know.

type family FieldBaseTypes (ftps :: Ctx (Symbol, Type)) :: Ctx WT.BaseType where
  FieldBaseTypes EmptyCtx = EmptyCtx
  FieldBaseTypes (ftps ::> ftp) = FieldBaseTypes ftps ::> FieldBaseType ftp

fieldBaseTypes :: Assignment FieldRepr ftps
               -> Assignment WT.BaseTypeRepr (FieldBaseTypes ftps)
fieldBaseTypes Empty = empty
fieldBaseTypes (ftps :> ftp) = fieldBaseTypes ftps :> fieldBaseType ftp

type family FieldBaseType (ftp :: (Symbol, Type)) :: WT.BaseType where
  FieldBaseType '(nm, tp) = TypeBaseType tp

fieldBaseType :: FieldRepr ftp -> WT.BaseTypeRepr (FieldBaseType ftp)
fieldBaseType (FieldRepr _ tp) = typeBaseType tp

type family TypesBaseTypes (tps :: Ctx Type) :: Ctx WT.BaseType where
  TypesBaseTypes EmptyCtx = EmptyCtx
  TypesBaseTypes (tps ::> tp) = TypesBaseTypes tps ::> TypeBaseType tp

typesBaseTypes :: Assignment TypeRepr tps
               -> Assignment WT.BaseTypeRepr (TypesBaseTypes tps)
typesBaseTypes Empty = Empty
typesBaseTypes (tps :> tp) = typesBaseTypes tps :> typeBaseType tp

type family TypeBaseType (tp :: Type) :: WT.BaseType where
  TypeBaseType BoolType = WT.BaseBoolType
  TypeBaseType IntType = WT.BaseIntegerType
  TypeBaseType (EnumType cs) = WT.BaseBVType (CtxSize cs)
  TypeBaseType (SetType cs) = WT.BaseBVType (CtxSize cs)
  TypeBaseType (StructType ftps) = WT.BaseStructType (FieldBaseTypes ftps)
  TypeBaseType (AbsType _) = WT.BaseIntegerType

typeBaseType :: TypeRepr tp -> WT.BaseTypeRepr (TypeBaseType tp)
typeBaseType BoolRepr          = WT.BaseBoolRepr
typeBaseType IntRepr           = WT.BaseIntegerRepr
typeBaseType (EnumRepr cs)     = WT.BaseBVRepr (ctxSizeNat (size cs))
typeBaseType (SetRepr cs)      = WT.BaseBVRepr (ctxSizeNat (size cs))
typeBaseType (StructRepr ftps) = WT.BaseStructRepr (fieldBaseTypes ftps)
typeBaseType (AbsRepr _)       = WT.BaseIntegerRepr

-- | Symbolic 'Literal'.
data SymLiteral t tp =
  SymLiteral { _symLiteralType :: TypeRepr tp
             , symLiteralExpr :: WB.Expr t (TypeBaseType tp)
             }

symLiteralExprs :: Assignment (SymLiteral t) tps
                -> Assignment (WB.Expr t) (TypesBaseTypes tps)
symLiteralExprs Empty = Empty
symLiteralExprs (symLits :> symLit) = symLiteralExprs symLits :> symLiteralExpr symLit

-- | Symbolic 'FieldLiteral'.
data SymFieldLiteral t (p :: (Symbol, Type)) where
  SymFieldLiteral :: { _symFieldLiteralName :: SymbolRepr nm
                     , _symFieldLiteralValue :: SymLiteral t tp
                     } -> SymFieldLiteral t '(nm, tp)

-- | Symbolic 'FunctionImpl'.
data SymFunction t fntp where
  SymFunction :: { symFunctionType :: FunctionTypeRepr (FunType nm args ret)
                 , symFunctionValue :: WB.ExprSymFn t (WB.Expr t) (TypesBaseTypes args) (TypeBaseType ret)
                 } -> SymFunction t (FunType nm args ret)

-- | Extract the What4 expression from a symbolic field literal.
symFieldLiteralExpr :: SymFieldLiteral t ftp -> WB.Expr t (FieldBaseType ftp)
symFieldLiteralExpr (SymFieldLiteral _ sl) = symLiteralExpr sl

-- | Declare a fresh uninterpreted 'SymFunction'.
freshUninterpSymFunction :: WB.ExprBuilder t st fs
                         -> FunctionTypeRepr fntp
                         -> IO (SymFunction t fntp)
freshUninterpSymFunction sym fntp@FunctionTypeRepr{..} = do
  let cSymbol = WI.safeSymbol (T.unpack (symbolRepr functionName))
      baseArgTypes = typesBaseTypes functionArgTypes
      baseRetType = typeBaseType functionRetType
  symFn <- WI.freshTotalUninterpFn sym cSymbol baseArgTypes baseRetType
  return $ SymFunction fntp symFn

freshSymLiteralConstants :: (AnyAbstractTypes ctx ~ 'False, WS.SMTLib2Tweaks solver)
                         => WB.ExprBuilder t st fs
                         -> WS.Session t solver
                         -> Assignment TypeRepr ctx
                         -> IO (Assignment (SymLiteral t) ctx)
freshSymLiteralConstants _ _ Empty = return Empty
freshSymLiteralConstants sym session (tps :> tp) = do
  (Refl, Refl) <- return $ noAbstractTypes (tps :> tp)
  r <- freshSymLiteralConstants sym session tps
  c <- freshSymLiteralConstant sym session "" tp
  return $ r :> c

-- TODO: After pulling out all the constraints into auxiliary functions we can
-- probably simplyify this one.
-- | Declare a fresh constant 'SymLiteral'. If this type is abstract, or
-- contains any abstract types, return 'Nothing'.
freshSymLiteralConstant :: (IsAbstractType tp ~ 'False, WS.SMTLib2Tweaks solver)
                        => WB.ExprBuilder t st fs
                        -> WS.Session t solver
                        -> String
                        -> TypeRepr tp
                        -> IO (SymLiteral t tp)
freshSymLiteralConstant sym session prefix tp = do
  let cSymbol = WI.safeSymbol prefix
  case tp of
    BoolRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseBoolRepr
      return $ SymLiteral tp c
    IntRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseIntegerRepr
      return $ SymLiteral tp c
    EnumRepr cs -> do
      let n = ctxSizeNat (size cs)
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      c_popcount <- WI.bvPopcount sym c
      one <- WI.bvLit sym n (BV.one n)
      c_popcount_1 <- WI.bvEq sym c_popcount one
      WS.assume (WS.sessionWriter session) c_popcount_1
      return $ SymLiteral tp c
    SetRepr cs -> do
      let n = ctxSizeNat (size cs)
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      return $ SymLiteral tp c
    StructRepr ftps -> do
      symExprs <- freshSymFieldLiteralExprs sym session prefix ftps
      structExpr <- WI.mkStruct sym symExprs
      return $ SymLiteral tp structExpr

freshSymFieldLiteralExprs :: (AnyAbstractFields ftps ~ 'False, WS.SMTLib2Tweaks solver)
                          => WB.ExprBuilder t st fs
                          -> WS.Session t solver
                          -> String
                          -> Assignment FieldRepr ftps
                          -> IO (Assignment (WB.Expr t) (FieldBaseTypes ftps))
freshSymFieldLiteralExprs _ _ _ Empty = return Empty
freshSymFieldLiteralExprs sym session prefix (ftps :> FieldRepr nm tp)
  | FalseRepr <- isAbstractType tp = do
      let prefix' = prefix ++ "." ++ T.unpack (symbolRepr nm)
      SymLiteral _ e <- freshSymLiteralConstant sym session prefix' tp
      symExprs <- freshSymFieldLiteralExprs sym session prefix ftps
      return $ symExprs :> e

-- | Check if two 'SymLiteral's are equal.
symLiteralEq :: forall t st fs tp .
                IsAbstractType tp ~ 'False
             => WB.ExprBuilder t st fs
             -> SymLiteral t tp
             -> SymLiteral t tp
             -> IO (WB.BoolExpr t)
symLiteralEq sym (SymLiteral tp e1) (SymLiteral _ e2) = case tp of
  BoolRepr -> WI.eqPred sym e1 e2
  IntRepr -> WI.intEq sym e1 e2
  EnumRepr _ -> WI.bvEq sym e1 e2
  SetRepr _ -> WI.bvEq sym e1 e2
  StructRepr _ -> WI.structEq sym e1 e2

-- | Inject a 'Literal' into a 'WB.Expr' by initiating the symbolic values to
-- concrete ones.
symExpr :: IsAbstractType tp ~ 'False
        => WB.ExprBuilder t st fs
        -> Literal tp
        -> IO (WB.Expr t (TypeBaseType tp))
symExpr sym l = case l of
  BoolLit True -> return $ WI.truePred sym
  BoolLit False -> return $ WI.falsePred sym
  IntLit x -> WI.intLit sym x
  EnumLit cs i -> do
    let n = ctxSizeNat (size cs)
    WI.bvLit sym n (indexBit n (Some i))
  SetLit cs is -> do
    let n = ctxSizeNat (size cs)
    WI.bvLit sym n (foldr BV.or (BV.zero n) (indexBit n <$> is))
  StructLit fls -> do
    symExprs <- symFieldLiteralExprs sym fls
    WI.mkStruct sym symExprs
  where indexBit n (Some i) = BV.bit' n (fromIntegral (indexVal i))

-- | Inject a 'Literal' into a 'SymLiteral' by initiating the symbolic values to
-- concrete ones.
symLiteral :: IsAbstractType tp ~ 'False
           => WB.ExprBuilder t st fs
           -> Literal tp
           -> IO (SymLiteral t tp)
symLiteral sym l = SymLiteral (literalType l) <$> symExpr sym l

_symLiterals :: AnyAbstractTypes tps ~ 'False
            => WB.ExprBuilder t st fs
            -> Assignment Literal tps
            -> IO (Assignment (SymLiteral t) tps)
_symLiterals _ Empty = return empty
_symLiterals sym (ls :> l)
  | FalseRepr <- isAbstractType (literalType l) = do
      sl <- symLiteral sym l
      sls <- _symLiterals sym ls
      return $ sls :> sl

-- | Convert an 'Assignment' of 'FieldLiteral's to an 'Assignment' of 'WB.Expr's
-- by calling 'symFieldLiteral' on each element.
symFieldLiteralExprs :: AnyAbstractFields ftps ~ 'False
                     => WB.ExprBuilder t st fs
                     -> Assignment FieldLiteral ftps
                     -> IO (Assignment (WB.Expr t) (FieldBaseTypes ftps))
symFieldLiteralExprs _ Empty = return empty
symFieldLiteralExprs sym (fls :> fl)
  | FalseRepr <- isAbstractField (fieldLiteralType fl) = do
      sfl <- symFieldLiteral sym fl
      sfls <- symFieldLiteralExprs sym fls
      return $ sfls :> symFieldLiteralExpr sfl

-- | Inject a 'FieldLiteral' into a 'SymFieldLiteral' by setting the
-- symbolic values equal to concrete ones.
symFieldLiteral :: IsAbstractField ftp ~ 'False
                => WB.ExprBuilder t st fs
                -> FieldLiteral ftp
                -> IO (SymFieldLiteral t ftp)
symFieldLiteral sym (FieldLiteral nm l) = SymFieldLiteral nm <$> symLiteral sym l

convertFieldIndex :: Index ftps ftp
                  -> Index (FieldBaseTypes ftps) (FieldBaseType ftp)
convertFieldIndex i = unsafeCoerce i

-- | Symbolically evaluate an expression given a symbolic instance.
symEvalExpr :: WB.ExprBuilder t st fs
            -> Assignment (SymFunction t) env
            -> Assignment (SymLiteral t) ctx
            -> Expr env ctx tp'
            -> IO (SymLiteral t tp')
symEvalExpr sym symFns symLits e = case e of
  LiteralExpr l -> symLiteral sym l
  VarExpr i -> return $ symLits ! i
  FieldExpr strE fi -> do
    SymLiteral (StructRepr ftps) str <- symEvalExpr sym symFns symLits strE
    fld <- WI.structField sym str (convertFieldIndex fi)
    return $ SymLiteral (fieldType (ftps ! fi)) fld
  ApplyExpr fi es -> do
    let SymFunction{..} = symFns ! fi
    args <- traverseFC (symEvalExpr sym symFns symLits) es
    ret <- WI.applySymFn sym symFunctionValue (symLiteralExprs args)
    return $ SymLiteral (functionRetType symFunctionType) ret
  EqExpr e1 e2 -> do
    sl1 <- symEvalExpr sym symFns symLits e1
    sl2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> symLiteralEq sym sl1 sl2
  LteExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.intLe sym sv1 sv2
  LtExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.intLt sym sv1 sv2
  GteExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.intLe sym sv2 sv1
  GtExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.intLt sym sv2 sv1
  PlusExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral IntRepr <$> WI.intAdd sym sv1 sv2
  MinusExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral IntRepr <$> WI.intSub sym sv1 sv2
  TimesExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral IntRepr <$> WI.intMul sym sv1 sv2
  ModExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral IntRepr <$> WI.intMod sym sv1 sv2
  DivExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symFns symLits e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symFns symLits e2
    SymLiteral IntRepr <$> WI.intDiv sym sv1 sv2
  NegExpr e' -> do
    SymLiteral IntRepr sv <- symEvalExpr sym symFns symLits e'
    SymLiteral IntRepr <$> WI.intNeg sym sv
  MemberExpr e1 e2 -> do
    SymLiteral (EnumRepr _) elt_bv <- symEvalExpr sym symFns symLits e1
    SymLiteral (SetRepr _) set_bv <- symEvalExpr sym symFns symLits e2
    elt_bv_and_set_bv <- WI.bvAndBits sym elt_bv set_bv
    elt_bv_in_set_bv <- WI.bvEq sym elt_bv elt_bv_and_set_bv
    return $ SymLiteral BoolRepr elt_bv_in_set_bv
  NotMemberExpr e1 e2 -> do
    SymLiteral (EnumRepr _) elt_bv <- symEvalExpr sym symFns symLits e1
    SymLiteral (SetRepr _) set_bv <- symEvalExpr sym symFns symLits e2
    elt_bv_and_set_bv <- WI.bvAndBits sym elt_bv set_bv
    elt_bv_notin_set_bv <- WI.bvNe sym elt_bv elt_bv_and_set_bv
    return $ SymLiteral BoolRepr elt_bv_notin_set_bv
  AndExpr e1 e2 -> do
    SymLiteral BoolRepr b1 <- symEvalExpr sym symFns symLits e1
    SymLiteral BoolRepr b2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.andPred sym b1 b2
  OrExpr e1 e2 -> do
    SymLiteral BoolRepr b1 <- symEvalExpr sym symFns symLits e1
    SymLiteral BoolRepr b2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.orPred sym b1 b2
  XorExpr e1 e2 -> do
    SymLiteral BoolRepr b1 <- symEvalExpr sym symFns symLits e1
    SymLiteral BoolRepr b2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.xorPred sym b1 b2
  ImpliesExpr e1 e2 -> do
    SymLiteral BoolRepr b1 <- symEvalExpr sym symFns symLits e1
    SymLiteral BoolRepr b2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.impliesPred sym b1 b2
  IffExpr e1 e2 -> do
    SymLiteral BoolRepr b1 <- symEvalExpr sym symFns symLits e1
    SymLiteral BoolRepr b2 <- symEvalExpr sym symFns symLits e2
    SymLiteral BoolRepr <$> WI.eqPred sym b1 b2
  NotExpr e' -> do
    SymLiteral BoolRepr b <- symEvalExpr sym symFns symLits e'
    SymLiteral BoolRepr <$> WI.notPred sym b

literalFromGroundValue :: TypeRepr tp
                       -> WT.BaseTypeRepr btp
                       -> WG.GroundValue btp
                       -> Maybe (Literal tp)
literalFromGroundValue tp btp val = case (tp, btp) of
  (BoolRepr, WT.BaseBoolRepr) -> Just $ BoolLit val
  (IntRepr, WT.BaseIntegerRepr) -> Just $ IntLit val
  (EnumRepr cs, WT.BaseBVRepr n)
    | Just Refl <- testEquality (ctxSizeNat (size cs)) n -> do
        ixNat <- return $ BV.asNatural (BV.ctz n val)
        Just (Some ix) <- return $ intIndex (fromIntegral ixNat) (size cs)
        return $ EnumLit cs ix
  (SetRepr cs, WT.BaseBVRepr n)
    | Just Refl <- testEquality (ctxSizeNat (size cs)) n -> do
        ixNats <- return $
          [ i | i' <- [0..toInteger (natValue n) - 1]
              , let i = fromInteger i'
              , BV.testBit' i val ]
        Just ixs <- return $
          sequence (map (flip intIndex (size cs)) (fromIntegral <$> ixNats))
        return $ SetLit cs ixs
  (StructRepr ftps, WT.BaseStructRepr btps) -> do
    lits <- literalsFromGroundValues' ftps btps val
    return $ StructLit lits
  _ -> Nothing

literalsFromGroundValues' :: Assignment FieldRepr ftps
                          -> Assignment WT.BaseTypeRepr btps
                          -> Assignment WG.GroundValueWrapper btps
                          -> Maybe (Assignment FieldLiteral ftps)
literalsFromGroundValues' Empty Empty Empty = Just Empty
literalsFromGroundValues' (ftps :> ftp@(FieldRepr _ _)) (btps :> btp) (gvs :> gv) =
  let mFls = literalsFromGroundValues' ftps btps gvs
      mFl = literalFromGroundValue' ftp btp (WG.unGVW gv)
  in case (mFls, mFl) of
       (Just fls, Just fl) -> Just $ fls :> fl
       _ -> Nothing
literalsFromGroundValues' _ _ _ = Nothing

-- | Like literalFromGroundValue, but with a 'FieldRepr'.
literalFromGroundValue' :: FieldRepr '(nm, tp)
                        -> WT.BaseTypeRepr btp
                        -> WG.GroundValue btp
                        -> Maybe (FieldLiteral '(nm, tp))
literalFromGroundValue' (FieldRepr nm tp) btp val =
  FieldLiteral nm <$> literalFromGroundValue tp btp val

groundEvalLiteral :: IsAbstractType tp ~ 'False
                  => WG.GroundEvalFn t
                  -> SymLiteral t tp
                  -> IO (Literal tp)
groundEvalLiteral WG.GroundEvalFn{..} (SymLiteral tp e) = case tp of
  BoolRepr -> BoolLit <$> groundEval e
  IntRepr -> IntLit <$> groundEval e
  EnumRepr cs -> do
    let n = ctxSizeNat (size cs)
    bv <- groundEval e
    ixNat <- return $ BV.asNatural (BV.ctz n bv)
    Just (Some ix) <- return $ intIndex (fromIntegral ixNat) (size cs)
    return $ EnumLit cs ix
  SetRepr cs -> do
    let n = ctxSizeNat (size cs)
    bv <- groundEval e
    ixNats <- return $
      [ i | i' <- [0..toInteger (natValue n) - 1]
          , let i = fromInteger i'
          , BV.testBit' i bv ]
    Just ixs <- return $
      sequence (map (flip intIndex (size cs)) (fromIntegral <$> ixNats))
    return $ SetLit cs ixs
  StructRepr ftps -> do
    gvws <- groundEval e
    case literalsFromGroundValues' ftps (fieldBaseTypes ftps) gvws of
      Just fls -> return $ StructLit fls
      Nothing -> error $
        "PANIC: Lobot.Instances.groundEvalLiteral: \n" ++ show ftps ++ "\n" ++ show (fieldBaseTypes ftps) ++ "\n" ++ show (ctxSizeNat (size gvws))

groundEvalLiterals :: AnyAbstractTypes ctx ~ 'False
                   => WG.GroundEvalFn t
                   -> Assignment TypeRepr ctx
                   -> Assignment (SymLiteral t) ctx
                   -> IO (Assignment Literal ctx)
groundEvalLiterals _ Empty Empty = return Empty
groundEvalLiterals ge (tps :> tp) (symLits :> symLit) = do
  (Refl, Refl) <- return $ noAbstractTypes (tps :> tp)
  ls <- groundEvalLiterals ge tps symLits
  l  <- groundEvalLiteral  ge symLit
  return $ ls :> l

data BuilderState s = EmptyBuilderState

data SessionData env ctx where
  SessionData :: (AnyAbstractTypes ctx ~ 'False, WS.SMTLib2Tweaks solver)
                 => { sym :: WB.ExprBuilder t st fs
                    , session :: WS.Session t solver
                    , env :: Assignment FunctionTypeRepr env
                    , fns :: Assignment (FunctionImpl IO) env
                    , tps :: Assignment TypeRepr ctx
                    , constraints :: [Expr env ctx BoolType]
                    , symFns :: Assignment (SymFunction t) env
                    , symLits :: Assignment (SymLiteral t) ctx
                    } -> SessionData env ctx

-- | The result of one round of instance generation. In the 'HasInstance' case,
-- we also include the list of constraints, if any, which are false after
-- evaluating all abstract functions, as well as the results of evaluating
-- each abstract function call.
data InstanceResult env ctx
  = HasInstance { inst :: Assignment Literal ctx
                , instFailedConstraints :: [Expr env ctx BoolType]
                , instFunctionCalls :: [FunctionCallResult env ctx] }
  | NoInstance
  | Unknown
  deriving Show

-- | A valid instance is one with no false constraints after evaluating all
-- abstract functions.
pattern ValidInstance :: Assignment Literal ctx
                      -> [FunctionCallResult env ctx]
                      -> InstanceResult env ctx
pattern ValidInstance ls calls = HasInstance ls [] calls

-- | An invalid instance is one with at least one false constraint after
-- evaluating all abstract functions.
pattern InvalidInstance :: Assignment Literal ctx
                        -> Expr env ctx BoolType -> [Expr env ctx BoolType]
                        -> [FunctionCallResult env ctx]
                        -> InstanceResult env ctx
pattern InvalidInstance ls fcn fcns calls = HasInstance ls (fcn:fcns) calls

-- | If is an instance in the current session, retrieve it, and then negate
-- that instance so we get a different result next time. Furthermore, we also
-- collect all concrete function calls evaluated during the course of this
-- check and add the results to the SMT solver's assumptions. This has the
-- effect of "teaching" the solver about the pointwise values of the functions.
-- We also distinguish between valid and invalid instances based on whether
-- the instance satisfies the given function environment.
getNextInstance :: forall env ctx. SessionData env ctx -> IO (InstanceResult env ctx)
getNextInstance SessionData{..} =
  WS.runCheckSat session $ \result ->
  case result of
    WS.Sat (ge,_) -> do
      ls <- groundEvalLiterals ge tps symLits
      let negateLiteral :: AnyAbstractTypes ctx ~ 'False
                        => Index ctx tp -> Literal tp -> IO [Expr env ctx BoolType]
          negateLiteral i l = do
            Refl <- return $ noAbstractTypesIx tps i
            return $ [NotExpr (EqExpr (VarExpr i) (LiteralExpr l))]
      negateExprs <- traverseAndCollect negateLiteral ls
      let negateExpr = foldr OrExpr (LiteralExpr (BoolLit False)) negateExprs
      SymLiteral BoolRepr symConstraint <- symEvalExpr sym symFns symLits negateExpr
      WS.assume (WS.sessionWriter session) symConstraint
      (fcns, calls) <- getFailingConstraints fns ls constraints
      traverse_ (assumeCall sym session symFns symLits) calls
      return (HasInstance ls fcns calls)
    WS.Unsat _ -> do return NoInstance
    WS.Unknown -> do return Unknown

runSession :: AnyAbstractTypes ctx ~ 'False
           => FilePath
           -- ^ Path to z3 executable
           -> Assignment FunctionTypeRepr env
           -- ^ Type of function environment
           -> Assignment (FunctionImpl IO) env
           -- ^ Concrete functions
           -> Assignment TypeRepr ctx
           -- ^ Types we are generating instances of
           -> [Expr env ctx BoolType]
           -- ^ Constraints
           -> (SessionData env ctx -> IO a)
           -- ^ Action to run
           -> IO a
runSession z3_path env fns tps constraints action = do
  Some nonceGen <- newIONonceGenerator
  sym <- WB.newExprBuilder WB.FloatIEEERepr EmptyBuilderState nonceGen
  WC.extendConfig WS.z3Options (WI.getConfiguration sym)
  WS.withZ3 sym z3_path WS.defaultLogData $ \session -> do
    symLits <- freshSymLiteralConstants sym session tps
    symFns <- traverseFC (freshUninterpSymFunction sym) env
    forM_ constraints $ \e -> do
      SymLiteral BoolRepr symConstraint <- symEvalExpr sym symFns symLits e
      WS.assume (WS.sessionWriter session) symConstraint
    action (SessionData sym session env fns tps constraints symFns symLits)

-- | Collect instances of a context of types satisfying a set of constraints,
-- only returning those instances that satisfy the given function environment.
-- Each time the solver returns an instance, we check whether it is actually an
-- instance and discard it if not; we also collect all concrete function calls
-- evaluated during the course of this check and add the results to the SMT
-- solver's assumptions. This has the effect of "teaching" the solver about the
-- pointwise values of the functions.
--
-- Along with the satisfying instances, return the number of total instances the
-- SMT solver returned (including the spurious ones).
collectAndFilterInstances :: Natural -> SessionData env ctx
                          -> IO ([Assignment Literal ctx], Natural)
collectAndFilterInstances 0 _ = return ([], 0)
collectAndFilterInstances limit s = do
  r <- getNextInstance s
  case r of
    ValidInstance ls _ -> do
      (lss, n) <- collectAndFilterInstances (limit-1) s
      return (ls:lss, n+1)
    InvalidInstance _ _ _ _ -> do
      (lss, n) <- collectAndFilterInstances (limit-1) s
      return (lss, n+1)
    _ -> return ([], 0)

-- | Assumes the result of a function call. If any of the functions arguments
-- are abstract, or if its return type is abstract, this is a no-op.
assumeCall :: WS.SMTLib2Tweaks solver
           => WB.ExprBuilder t st fs
           -> WS.Session t solver
           -> Assignment (SymFunction t) env
           -> Assignment (SymLiteral t) ctx
           -> FunctionCallResult env ctx
           -> IO ()
assumeCall sym session symFns symLits (FunctionCallResult fi args ret _)
  | SymFunction{..} <- symFns ! fi = do
      symArgs  <- traverseFC (symEvalExpr sym symFns symLits) args
      symRet   <- symLiteral sym ret
      symApply <- WI.applySymFn sym symFunctionValue (symLiteralExprs symArgs)
      symRes <- case functionRetType symFunctionType of
        BoolRepr     -> WI.eqPred   sym symApply (symLiteralExpr symRet)
        IntRepr      -> WI.intEq    sym symApply (symLiteralExpr symRet)
        EnumRepr   _ -> WI.bvEq     sym symApply (symLiteralExpr symRet)
        SetRepr    _ -> WI.bvEq     sym symApply (symLiteralExpr symRet)
        StructRepr _ -> WI.structEq sym symApply (symLiteralExpr symRet)
      WS.assume (WS.sessionWriter session) symRes
