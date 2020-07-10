{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Main
Description : The Lobot command line interface
Copyright   : (c) Matthew Yacavone, Ben Selfridge, 2020
License     : BSD3
Maintainer  : myac@galois.com
Stability   : experimental
Portability : POSIX

This module provides the main command line interface for Lobot. This code is
currently very messy, and need to be cleaned up and documented.
-}
module Main
  ( main
  ) where

import Lobot.Expr
import Lobot.Instances
import Lobot.JSON
import Lobot.Kind
import Lobot.Parser
import Lobot.Pretty
import Lobot.TypeCheck
import Lobot.Types
import Lobot.Utils

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashSet as HS
import qualified Text.PrettyPrint as PP
import qualified Data.Text as T
import Data.Text (Text)

import Data.List (find)
import Control.Monad (void, when)
import Data.Maybe
import Data.Foldable (forM_)
import Data.Functor.Const
import Data.Parameterized.BoolRepr
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Numeric.Natural

import Options.Applicative

import System.IO (hFlush, stdout)
import System.Console.ANSI (hSupportsANSI, clearLine, setCursorColumn)
import System.Process hiding (env)
import System.Directory
import System.Exit

putStrNow :: String -> IO ()
putStrNow s = putStr s >> hFlush stdout


data FileInputOptions = BrowseKindInsts Text Bool
                      | GenKindInsts Text
                      | RunCheck Text
                      | RunAllChecks
                      deriving Show

fileInputOptions :: Parser FileInputOptions
fileInputOptions = (BrowseKindInsts . T.pack
                    <$> strOption (long "enumerate" <> short 'e'
                                   <> metavar "KIND"
                                   <> help "Browse all instances of a given kind.")
                    <*> switch (long "verbose" <> short 'v'
                                <> help "Show additional output while browsing instances."))
               <|> (GenKindInsts . T.pack
                    <$> strOption (long "count" <> short 'c'
                                   <> metavar "KIND"
                                   <> help "Generate and count all instances of a given kind."))
               <|> (RunCheck . T.pack
                    <$> strOption (long "run" <> short 'r'
                                   <> metavar "CHECK"
                                   <> help "Run a given check."))
               <|> (const RunAllChecks
                    <$> switch (long "run-all" <> short 'a'
                                <> help "Run all checks in the given file."))

data Options = Options { inFileName :: String
                       , inLimit :: Natural
                       , inOptions :: FileInputOptions }
  deriving Show

options :: Parser Options
options = Options <$> argument str (metavar "FILE")
                  <*> option auto (long "limit" <> short 'l' <> value 100
                                   <> metavar "INT"
                                   <> help "Set the maximum number of instances to generate.")
                  <*> fileInputOptions

main :: IO ()
main = lobot =<< execParser i
  where i = info (options <**> helper)
          ( fullDesc <> progDesc "If no options are given, run all checks in the given file."
          )

lobot :: Options -> IO ()
lobot Options{..} = do
  mz3 <- findExecutable "z3"
  when (not (isJust mz3)) $ do
    putStrLn $ "ERROR -- z3 executable must be on your path."
    exitFailure
  let Just z3 = mz3
  fileStr <- readFile inFileName
  case parseDecls inFileName fileStr of
    Left err -> putStrLn err
    Right decls -> case typeCheck decls of
      Left err -> print $ ppTypeError inFileName err
      Right (TypeCheckResult env ks cks, ws) -> do
        forM_ ws $ print . ppTypeWarning inFileName
        case inOptions of
          BrowseKindInsts k verbose -> do
            SomeNonAbsKind tp cns <- lookupKind k ks
            runSession z3 env (canonicalEnv env) (Empty :> tp) cns
                       (browseKindInstances k verbose)
          GenKindInsts k -> do
            SomeNonAbsKind tp cns <- lookupKind k ks
            void $ runSession z3 env (canonicalEnv env) (Empty :> tp) cns
                             (generateKindInstances k inLimit)
          RunCheck ck -> do
            SomeNonAbsCheck _ fldnms tps cns <- lookupCheck ck cks
            void $ runSession z3 env (canonicalEnv env) tps cns
                             (runCheck ck fldnms inLimit)
          RunAllChecks -> do
            let go :: Assignment FunctionTypeRepr env -> Some (Check env) -> IO Bool
                go env' some_ck = do 
                  SomeNonAbsCheck nm fldnms tps cns <- toNonAbstractCheck some_ck;
                  isNothing <$> runSession z3 env' (canonicalEnv env') tps cns
                                           (runCheck nm fldnms inLimit)
            bs <- mapM (go env) cks
            if null bs then putStrLn "All checks pass. (File had no checks)"
            else if and bs then putStrLn "All checks pass."
            else return ()

data SomeNonAbstractKind env where
  SomeNonAbsKind :: IsAbstractType tp ~ 'False
                 => TypeRepr tp
                 -> [KindExpr env tp BoolType]
                 -> SomeNonAbstractKind env

lookupKind :: Text -> [Some (Kind env)] -> IO (SomeNonAbstractKind env)
lookupKind nm ks = case find (\(Some k) -> kindName k == nm) ks of
  Nothing -> do putStrLn $ "Kind '" ++ T.unpack nm ++ "' not found."
                exitFailure
  Just (Some k) -> case isAbstractType (kindType k) of
    TrueRepr -> do putStrLn $ "Cannot generate instances of abstract type."
                   exitFailure
    FalseRepr -> pure $ SomeNonAbsKind (kindType k) (kindConstraints k)

data SomeNonAbstractCheck env where
  SomeNonAbsCheck :: AnyAbstractTypes tps ~ 'False
                  => Text -> [Text]
                  -> Assignment TypeRepr tps
                  -> [Expr env tps BoolType]
                  -> SomeNonAbstractCheck env

lookupCheck :: Text -> [Some (Check env)] -> IO (SomeNonAbstractCheck env)
lookupCheck nm cks = case find (\(Some ck) -> checkName ck == nm) cks of
  Nothing -> do putStrLn $ "Check '" ++ T.unpack nm ++ "' not found."
                exitFailure
  Just some_ck -> toNonAbstractCheck some_ck

toNonAbstractCheck :: Some (Check env) -> IO (SomeNonAbstractCheck env)
toNonAbstractCheck (Some ck) =
  let fldnms = toListFC namedTypeName (checkFields ck)
      tps    = fmapFC   namedTypeType (checkFields ck)
  in case anyAbstractTypes tps of
    TrueRepr -> do putStrLn $ "Cannot generate instances of abstract type."
                   exitFailure
    FalseRepr -> do
      let cns = checkConstraints ck ++ [negReqs]
          negReqs = NotExpr (foldr AndExpr (LiteralExpr (BoolLit True))
                                   (checkRequirements ck))
      pure $ SomeNonAbsCheck (checkName ck) fldnms tps cns


browseKindInstances :: forall env tp. Text -> Bool -> SessionData env (EmptyCtx ::> tp) -> IO ()
browseKindInstances k verbose s@SessionData{..} =
  void $ generateInstances k Nothing onInst s
  where onInst :: InstanceResult env (EmptyCtx ::> tp)
               -> [FunctionCallResult env (EmptyCtx ::> tp)]
               -> Natural -> Natural -> IO Bool
        onInst (HasInstance (Empty :> l) fcns calls) calls' n _
          | null fcns || verbose = do
            if null fcns then putStrLn $ "Instance " ++ show n ++ ":"
                         else putStrLn $ "Generated an invalid instance:"
            print . PP.nest 2 $ ppLiteralWithKindName k l
            when (not (null fcns)) $ do
              putStrLn "The constraints that failed were:"
              forM_ fcns (\c -> print . PP.nest 2 $ ppExpr env tps (Empty :> Const "self") c)
            when (verbose && not (null calls')) $ do
              putStrLn "Learned the values of the following function calls:"
              forM_ calls' (\c -> print . PP.nest 2 $ ppFunctionCallResult env tps (Empty :> Const "self") c)
            let outps = mapMaybe (\(FunctionCallResult fi args _ st) ->
                          if null st then Nothing
                          else Just (ppExpr env tps (Empty :> Const "self")
                                            (ApplyExpr fi args), st)) calls
            when (verbose && not (null outps)) $ do
              putStrLn "The following function calls generated output:"
              forM_ outps (\(d,st) -> print . PP.nest 2 $ d PP.<> PP.colon PP.<+> PP.text st)
            putStrLn "Press enter to see the next instance" -- , or q to quit."
            untilJust $ getChar >>= \case
                          '\n' -> pure $ Just True
                          -- 'Q'  -> pure $ Just False
                          -- 'q'  -> pure $ Just False
                          _    -> pure $ Nothing
          | otherwise = pure True
        onInst _ _ 0 ivis = do
          putStrLn $ "Found no valid instances! (Generated "
                     ++ show ivis ++ " invalid instances)"
          pure False
        onInst _ _ vis ivis = do
          putStrLn $ "Enumerated all " ++ show vis ++ " valid instances, "
                     ++ "generated " ++ show ivis ++ " invalid instances"
          pure False
          
generateKindInstances :: Text -> Natural -> SessionData env (EmptyCtx ::> tp)
                      -> IO ([Assignment Literal (EmptyCtx ::> tp)], Natural)
generateKindInstances k ilimit =
  generateInstances k (Just (ilimit, onLimit)) onInst
  where onInst :: InstanceResult env (EmptyCtx ::> tp)
               -> [FunctionCallResult env ctx] -> Natural -> Natural -> IO Bool
        onInst (HasInstance _ _ _) _ _ _ = pure True
        onInst _ _ 0 ivis = do
          putStrLn $ "Found no valid instances! (Generated "
                     ++ show ivis ++ " invalid instances)"
          pure False
        onInst _ _ vis ivis = do
          putStrLn $ "Found " ++ show vis
                     ++ " valid instances, generated " ++ show ivis
                     ++ " invalid instances"
          pure False
        onLimit :: Natural -> Natural -> IO ()
        onLimit vis ivis = do
          putStrLn $ "Hit instance limit of " ++ show ilimit ++ "!"
          putStrLn $ "Found " ++ show vis
                     ++ " valid instances, generated " ++ show ivis
                     ++ " invalid instances"

runCheck :: Text -> [Text] -> Natural -> SessionData env ctx
         -> IO (Maybe (Assignment Literal ctx))
runCheck ck fldnms ilimit s = do 
  (ls,_) <- generateInstances ck (Just (ilimit, onLimit)) onInst s
  pure $ listToMaybe ls
  where onInst :: InstanceResult env ctx
               -> [FunctionCallResult env ctx] -> Natural -> Natural -> IO Bool
        onInst (ValidInstance ls _) _ _ _ = do
          putStrLn $ "Check '" ++ T.unpack ck ++ "' failed with counterexample:"
          forM_ (zip fldnms (toListFC Some ls)) $ \(fldnm, Some l) ->
            putStrLn $ "  " ++ T.unpack fldnm ++ " = " ++ show (ppLiteral l)
          pure False
        onInst (InvalidInstance _ _ _ _) _ _ _ = pure True
        onInst _ _ vis ivis = do
          putStrLn $ "Check '" ++ T.unpack ck ++ "' holds. "
                     ++ "(Generated " ++ show (vis+ivis) ++ " instances)"
          pure False
        onLimit :: Natural -> Natural -> IO ()
        onLimit vis ivis = do
          putStrLn $ "Check '" ++ T.unpack ck ++ "' holds for the first "
                     ++ show (vis+ivis) ++ " generated instances."

generateInstances :: forall env ctx. Text
                  -> Maybe (Natural, Natural -> Natural -> IO ())
                  -> (InstanceResult env ctx -> [FunctionCallResult env ctx]
                                             -> Natural -> Natural -> IO Bool)
                  -> SessionData env ctx
                  -> IO ([Assignment Literal ctx], Natural)
generateInstances nm mb_limit onInst s@SessionData{..} = do
  useANSI <- hSupportsANSI stdout
  if useANSI then putStrNow (msg 0 0)
             else putStrLn "Generating instances..."
  go useANSI HS.empty 0 0
  where go :: Bool -> HS.Set (FunctionCallResult env ctx) -> Natural -> Natural
           -> IO ([Assignment Literal ctx], Natural)
        go useANSI _ vis ivis | Just (ilimit, onLimit) <- mb_limit
                              , vis + ivis >= ilimit = do
          when useANSI $ clearLine >> setCursorColumn 0
          onLimit vis ivis
          pure ([], vis+ivis)
        go useANSI call_set vis ivis | otherwise =
          getNextInstance s >>= \case
            HasInstance ls fcns calls -> do
              let (toAdd, vis', ivis') = case null fcns of
                    True  -> ([ls], vis+1, ivis) -- ls is a valid instance
                    False -> ([],   vis, ivis+1) -- ls is an invalid instance
              -- only pass calls we haven't seen before to onInst
              let calls' = filter (`HS.notMember` call_set) calls
              when useANSI $ clearLine >> setCursorColumn 0
              cont <- onInst (HasInstance ls fcns calls) calls' vis' ivis'
              if cont then do
                when useANSI $ putStrNow (msg vis' ivis')
                let call_set' = foldr HS.insert call_set calls'
                (lss, tot) <- go useANSI call_set' vis' ivis'
                return (toAdd ++ lss, tot)
              else return (toAdd, vis'+ivis')
            ir -> do
              when useANSI $ clearLine >> setCursorColumn 0
              _ <- onInst ir [] vis ivis
              return ([], vis+ivis)
        msg :: Natural -> Natural -> String
        msg 0 0      = "Generating instances of '" ++ T.unpack nm ++ "'..."
        msg vis ivis = msg 0 0 ++ " | Found " ++ show vis
                       ++ " valid instances, " ++ show ivis
                       ++ " invalid instances"


canonicalEnv :: Assignment FunctionTypeRepr fntps
             -> Assignment (FunctionImpl IO) fntps
canonicalEnv Empty = Empty
canonicalEnv (fntps :> fntp@FunctionTypeRepr{}) = canonicalEnv fntps :> canonicalFn fntp

canonicalFn :: FunctionTypeRepr (FunType nm args ret)
            -> FunctionImpl IO (FunType nm args ret)
canonicalFn fntp = FunctionImpl fntp (run fntp)

run :: FunctionTypeRepr (FunType nm args ret)
    -> Assignment Literal args
    -> IO (Literal ret, String)
run FunctionTypeRepr{..} args = do
  let json_args = A.toJSONList (toListFC literalToJSON args)
      std_in = BS.unpack (A.encode json_args)
      p = shell (T.unpack (symbolRepr functionName))
  (ec, std_out, std_err) <- readCreateProcessWithExitCode p std_in
  case ec of
    ExitSuccess -> case A.eitherDecode (BS.pack std_out) of
      Right v -> case literalFromJSON v of
        A.Success (Some l') -> case testEquality functionRetType (literalType l') of
          Just Refl -> return (l', std_err)
          Nothing -> do putStrLn $ "expected " ++ show functionRetType ++ ", got " ++ show l'
                        exitFailure
        A.Error e -> do putStrLn $ "error: " ++ e
                        exitFailure
      Left e -> do putStrLn "Error decoding JSON"
                   putStrLn std_out
                   putStrLn e
                   exitFailure
    ExitFailure _ -> do putStrLn $ "error: " ++ std_err
                        exitFailure
