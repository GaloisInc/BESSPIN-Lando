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

import Lobot.Core.Kind
import Lobot.Core.Kind.Pretty

import qualified Data.BitVector.Sized    as BV
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
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Prelude hiding ((!!))
import Unsafe.Coerce (unsafeCoerce) -- I know, I know.

ctxSize :: Assignment f tps -> NatRepr (CtxSize tps)
ctxSize Empty = knownNat
ctxSize (fs :> _) = (knownNat @1) `addNat` ctxSize fs

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

type family TypeBaseType (tp :: Type) :: WT.BaseType where
  TypeBaseType BoolType = WT.BaseBoolType
  TypeBaseType IntType = WT.BaseIntegerType
  TypeBaseType (EnumType cs) = WT.BaseBVType (CtxSize cs)
  TypeBaseType (SetType cs) = WT.BaseBVType (CtxSize cs)
  TypeBaseType (StructType ftps) = WT.BaseStructType (FieldBaseTypes ftps)

typeBaseType :: TypeRepr tp -> WT.BaseTypeRepr (TypeBaseType tp)
typeBaseType BoolRepr = WT.BaseBoolRepr
typeBaseType IntRepr = WT.BaseIntegerRepr
typeBaseType (EnumRepr cs) = WT.BaseBVRepr (ctxSize cs)
typeBaseType (SetRepr cs) = WT.BaseBVRepr (ctxSize cs)
typeBaseType (StructRepr ftps) = WT.BaseStructRepr (fieldBaseTypes ftps)

-- | Symbolic 'Literal'.
data SymLiteral t tp =
  SymLiteral { symLiteralType :: TypeRepr tp
             , symLiteralExpr :: WB.Expr t (TypeBaseType tp)
             }

-- | Symbolic 'FieldLiteral'.
data SymFieldLiteral t (p :: (Symbol, Type)) where
  SymFieldLiteral :: { symFieldLiteralName :: SymbolRepr nm
                     , symFieldLiteralValue :: SymLiteral t tp
                     } -> SymFieldLiteral t '(nm, tp)

-- | Extract the What4 expression from a symbolic field literal.
symFieldLiteralExpr :: SymFieldLiteral t ftp -> WB.Expr t (FieldBaseType ftp)
symFieldLiteralExpr (SymFieldLiteral _ sl) = symLiteralExpr sl

-- | Declare a fresh constant 'SymLiteral'.
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
      return $ SymLiteral tp c
    IntRepr -> do
      c <- WI.freshConstant sym cSymbol WT.BaseIntegerRepr
      return $ SymLiteral tp c
    EnumRepr cs -> do
      -- We represent individual enumeration constants as 1 << n for some n.
      -- Therefore, we add a constraint that the popcount of the bitvector is 1.
      let n = ctxSize cs
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      c_popcount <- WI.bvPopcount sym c
      one <- WI.bvLit sym n (BV.one n)
      c_popcount_1 <- WI.bvEq sym c_popcount one
      WS.assume (WS.sessionWriter session) c_popcount_1
      return $ SymLiteral tp c
    SetRepr cs -> do
      let n = ctxSize cs
      c <- WI.freshConstant sym cSymbol (WT.BaseBVRepr n)
      return $ SymLiteral tp c
    StructRepr flds -> do
      c <- WI.freshConstant sym cSymbol (WT.BaseStructRepr (fieldBaseTypes flds))
      return $ SymLiteral tp c

-- | Check if two 'SymLiteral's are equal.
symLiteralEq :: forall t st fs tp .
                WB.ExprBuilder t st fs
             -> SymLiteral t tp
             -> SymLiteral t tp
             -> IO (WB.BoolExpr t)
symLiteralEq sym (SymLiteral tp e1) (SymLiteral _ e2) = case tp of
  BoolRepr -> WI.eqPred sym e1 e2
  IntRepr -> WI.intEq sym e1 e2
  EnumRepr _ -> WI.bvEq sym e1 e2
  SetRepr _ -> WI.bvEq sym e1 e2
  StructRepr _ -> WI.structEq sym e1 e2

-- | Convert an 'Assignment' of 'FieldLiteral's to an 'Assignment' of 'WB.Expr's by calling 'symFieldLiteral' on each element.
symFieldLiteralExprs :: WB.ExprBuilder t st fs
                     -> Assignment FieldLiteral ftps
                     -> IO (Assignment (WB.Expr t) (FieldBaseTypes ftps))
symFieldLiteralExprs _ Empty = return empty
symFieldLiteralExprs sym (fls :> fl) = do
  sfl <- symFieldLiteral sym fl
  sfls <- symFieldLiteralExprs sym fls
  return $ sfls :> symFieldLiteralExpr sfl

-- | Inject a 'Literal' into a 'WB.Expr' by initiating the symbolic values to
-- concrete ones.
symExpr :: WB.ExprBuilder t st fs -> Literal tp -> IO (WB.Expr t (TypeBaseType tp))
symExpr sym l = case l of
  BoolLit True -> return $ WI.truePred sym
  BoolLit False -> return $ WI.falsePred sym
  IntLit x -> WI.intLit sym x
  EnumLit cs i -> do
    let n = ctxSize cs
    WI.bvLit sym n (indexBit n (Some i))
  SetLit cs is -> do
    let n = ctxSize cs
    WI.bvLit sym n (foldr BV.or (BV.zero n) (indexBit n <$> is))
  StructLit fls -> do
    symExprs <- symFieldLiteralExprs sym fls
    WI.mkStruct sym symExprs
  where indexBit n (Some i) = BV.bit' n (fromIntegral (indexVal i))

-- | Inject a 'Literal' into a 'SymLiteral' by initiating the symbolic values to
-- concrete ones.
symLiteral :: WB.ExprBuilder t st fs -> Literal tp -> IO (SymLiteral t tp)
symLiteral sym l = SymLiteral (literalType l) <$> symExpr sym l

-- | Inject a 'FieldLiteral' into a 'SymFieldLiteral' by setting the
-- symbolic values equal to concrete ones.
symFieldLiteral :: WB.ExprBuilder t st fs
                -> FieldLiteral ftp
                -> IO (SymFieldLiteral t ftp)
symFieldLiteral sym (FieldLiteral nm l) = SymFieldLiteral nm <$> symLiteral sym l

convertFieldIndex :: Index ftps ftp
                  -> Index (FieldBaseTypes ftps) (FieldBaseType ftp)
convertFieldIndex i = unsafeCoerce i

-- | Symbolically evaluate an expression given a symbolic instance.
symEvalExpr :: WB.ExprBuilder t st fs
            -> SymLiteral t tp
            -> Expr env tp tp'
            -> IO (SymLiteral t tp')
symEvalExpr sym symLit e = case e of
  LiteralExpr l -> symLiteral sym l
  SelfExpr -> return symLit
  FieldExpr strE fi -> do
    SymLiteral (StructRepr ftps) str <- symEvalExpr sym symLit strE
    fld <- WI.structField sym str (convertFieldIndex fi)
    return $ SymLiteral (fieldType (ftps ! fi)) fld
  EqExpr e1 e2 -> do
    sl1 <- symEvalExpr sym symLit e1
    sl2 <- symEvalExpr sym symLit e2
    SymLiteral BoolRepr <$> symLiteralEq sym sl1 sl2
  LteExpr e1 e2 -> do
    SymLiteral IntRepr sv1 <- symEvalExpr sym symLit e1
    SymLiteral IntRepr sv2 <- symEvalExpr sym symLit e2
    SymLiteral BoolRepr <$> WI.intLe sym sv1 sv2
  MemberExpr e1 e2 -> do
    SymLiteral (EnumRepr _) elt_bv <- symEvalExpr sym symLit e1
    SymLiteral (SetRepr _) set_bv <- symEvalExpr sym symLit e2
    elt_bv_and_set_bv <- WI.bvAndBits sym elt_bv set_bv
    elt_bv_in_set_bv <- WI.bvEq sym elt_bv elt_bv_and_set_bv
    return $ SymLiteral BoolRepr elt_bv_in_set_bv
  ImpliesExpr e1 e2 -> do
    SymLiteral BoolRepr b1 <- symEvalExpr sym symLit e1
    SymLiteral BoolRepr b2 <- symEvalExpr sym symLit e2
    SymLiteral BoolRepr <$> WI.impliesPred sym b1 b2
  NotExpr e' -> do
    SymLiteral BoolRepr b <- symEvalExpr sym symLit e'
    SymLiteral BoolRepr <$> WI.notPred sym b

literalFromGroundValue :: TypeRepr tp
                       -> WT.BaseTypeRepr btp
                       -> WG.GroundValue btp
                       -> Maybe (Literal tp)
literalFromGroundValue tp btp val = case (tp, btp) of
  (BoolRepr, WT.BaseBoolRepr) -> Just $ BoolLit val
  (IntRepr, WT.BaseIntegerRepr) -> Just $ IntLit val
  (EnumRepr cs, WT.BaseBVRepr n)
    | Just Refl <- testEquality (ctxSize cs) n -> do
        ixNat <- return $ BV.asNatural (BV.ctz n val)
        Just (Some ix) <- return $ intIndex (fromIntegral ixNat) (size cs)
        return $ EnumLit cs ix
  (SetRepr cs, WT.BaseBVRepr n)
    | Just Refl <- testEquality (ctxSize cs) n -> do
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

groundEvalLiteral :: WG.GroundEvalFn t
                  -> SymLiteral t tp
                  -> IO (Literal tp)
groundEvalLiteral WG.GroundEvalFn{..} (SymLiteral tp e) = case tp of
  BoolRepr -> BoolLit <$> groundEval e
  IntRepr -> IntLit <$> groundEval e
  EnumRepr cs -> do
    let n = ctxSize cs
    bv <- groundEval e
    ixNat <- return $ BV.asNatural (BV.ctz n bv)
    Just (Some ix) <- return $ intIndex (fromIntegral ixNat) (size cs)
    return $ EnumLit cs ix
  SetRepr cs -> do
    let n = ctxSize cs
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
      Nothing -> error "PANIC: Lobot.Core.Instances.groundEvalLiteral"

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
      SymLiteral BoolRepr symConstraint <- symEvalExpr sym symLit e
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
      SymLiteral BoolRepr symConstraint <- symEvalExpr sym symLit negateExpr
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
      SymLiteral BoolRepr symConstraint <- symEvalExpr sym symLit e
      WS.assume (WS.sessionWriter session) symConstraint
    k sym session symLit
