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

This module provides the main command line interface for Lobot.
-}
module Main
  ( main
  , FileInputOptions(..)
  , Options(..)
  , options
  , lobot
  ) where

import Lobot.Expr
import Lobot.Instances
import Lobot.JSON
import Lobot.Kind
import Lobot.Parser
import Lobot.Pretty
import Lobot.TypeCheck
import Lobot.Types

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashSet as HS
import qualified Text.PrettyPrint as PP
import qualified Data.Text as T
import Data.Text (Text)

import Data.List (find)
import Control.Monad (void, when, unless)
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
import System.Console.ANSI (hSupportsANSI, clearLine, setCursorColumn, cursorUp)
import System.Process hiding (env)
import System.Directory
import System.Exit

-- | Print a string without a trailing newline and show it immediately
putStrNow :: String -> IO ()
putStrNow s = putStr s >> hFlush stdout

-- | Perform the given action only when ANSI features are enabled
whenANSI :: IO () -> IO ()
whenANSI m = hSupportsANSI stdout >>= flip when m

-- | Perform the given action only when ANSI features are disabled
whenNoANSI :: IO () -> IO ()
whenNoANSI m = hSupportsANSI stdout >>= flip unless m

-- | Clears the last @n@ lines in the terminal, moving the cursor to column 0
clearLinesWithCursor :: Int -> IO ()
clearLinesWithCursor = whenANSI . go
  where go n | n <= 0    = pure ()
             | n == 1    = clearLine >> setCursorColumn 0
             | otherwise = clearLine >> cursorUp 1 >> go (n-1)


-- | The available options when lobot is given an input file
data FileInputOptions = BrowseKindInsts Text Bool
                      -- ^ The kind name to browse instances of, and a boolean
                      -- flag indicating verbose mode
                      | GenKindInsts Text
                      -- ^ The kind name to count kind instances of
                      | RunCheck Text
                      -- ^ The check name to run
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

-- | All the command line options to lobot
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

-- | Run 'execParser' on 'options', then call 'lobot'
main :: IO ()
main = lobot =<< execParser i
  where i = info (options <**> helper)
          ( fullDesc <> progDesc "If no options are given, run all checks in the given file."
          )

-- | Run the lobot CLI on the given options
lobot :: Options -> IO ()
lobot Options{..} = do
  mz3 <- findExecutable "z3"
  when (not (isJust mz3)) $ do
    putStrLn $ "ERROR -- z3 executable must be on your path."
    exitFailure
  let Just z3 = mz3
  fileStr <- readFile inFileName
  case parseDecls inFileName fileStr of
    -- print any lexical/parse errors
    Left err -> do putStrLn err
                   exitFailure
    Right decls -> case typeCheck decls of
      -- print any type errors
      Left err -> do print $ ppTypeError inFileName err
                     exitFailure
      Right (TypeCheckResult env ks cks, ws) -> do
        -- print any type warnings
        forM_ ws $ print . ppTypeWarning inFileName
        case inOptions of
          BrowseKindInsts k verbose -> do
            SomeNonAbsKind tp cns <- lookupKind k ks
            putStrLn ""
            runSession z3 env (canonicalEnv env) (Empty :> tp) cns
                       (browseKindInstances k inLimit verbose)
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

-- | TODO Move to Kind.hs?
data SomeNonAbstractKind env where
  SomeNonAbsKind :: NonAbstract tp
                 => TypeRepr tp
                 -> [KindExpr env tp BoolType]
                 -> SomeNonAbstractKind env

-- | Tries to find a kind with the given name in the given list, erroring and
-- failing if the kind is not found, or is abstract.
lookupKind :: Text -> [Some (Kind env)] -> IO (SomeNonAbstractKind env)
lookupKind nm ks = case find (\(Some k) -> kindName k == nm) ks of
  Nothing -> do putStrLn $ "Kind '" ++ T.unpack nm ++ "' not found."
                exitFailure
  Just (Some k) -> case isNonAbstract (kindType k) of
    Nothing -> do putStrLn $ "Cannot generate instances of abstract type."
                  exitFailure
    Just IsNonAbs -> pure $ SomeNonAbsKind (kindType k) (kindConstraints k)

-- | TODO Move to Kind.hs?
data SomeNonAbstractCheck env where
  SomeNonAbsCheck :: NonAbstract tps
                  => Text -> [Text]
                  -> Assignment TypeRepr tps
                  -> [Expr env tps BoolType]
                  -> SomeNonAbstractCheck env

-- | Tries to find a check with the given name in the given list, erroring and
-- failing if the check is not found, or is abstract.
lookupCheck :: Text -> [Some (Check env)] -> IO (SomeNonAbstractCheck env)
lookupCheck nm cks = case find (\(Some ck) -> checkName ck == nm) cks of
  Nothing -> do putStrLn $ "Check '" ++ T.unpack nm ++ "' not found."
                exitFailure
  Just some_ck -> toNonAbstractCheck some_ck

-- | Ensures the given check is non-abstract
toNonAbstractCheck :: Some (Check env) -> IO (SomeNonAbstractCheck env)
toNonAbstractCheck (Some ck) =
  let fldnms = toListFC namedTypeName (checkFields ck)
      tps    = fmapFC   namedTypeType (checkFields ck)
  in case isNonAbstract tps of
    Nothing -> do putStrLn $ "Cannot generate instances of abstract type."
                  exitFailure
    Just IsNonAbs -> do
      let cns = checkConstraints ck ++ [negReqs]
          negReqs = NotExpr (foldr AndExpr (LiteralExpr (BoolLit True))
                                   (checkRequirements ck))
      pure $ SomeNonAbsCheck (checkName ck) fldnms tps cns


-- | Given a kind name, an instance limit, a boolean indicating verbose mode,
-- and a solver session, this function enumerates and prints all valid
-- instances of the given kind using 'generateInstances'. If the given boolean
-- is true, also print all invalid instances, as well as all learned function
-- calls and all function call output.
browseKindInstances :: forall env tp. Text -> Natural -> Bool
                    -> SessionData env (EmptyCtx ::> tp) -> IO ()
browseKindInstances k limit verbose s@SessionData{..} =
  void $ generateInstances msg limit onLimit onInst s
  where msg = "Generating instances of '" ++ T.unpack k ++ "'..."
        onInst :: InstanceResult env (EmptyCtx ::> tp)
               -> HS.HashSet (FunctionCallResult env (EmptyCtx ::> tp))
               -> Natural -> Natural -> IO Bool
        onInst (HasInstance (Empty :> l) fcns calls) calls' n _
          | null fcns || verbose = do
            if null fcns then putStrLn $ "Instance " ++ show n ++ ":"
                         else putStrLn $ "Generated an invalid instance:"
            print . PP.nest 2 $ ppLiteralWithKindName k l
            when (not (null fcns)) $ do
              putStrLn "The constraints that failed were:"
              forM_ fcns (\c -> print . PP.nest 2 $ ppExpr env tps (Empty :> Const "self") c)
            when (verbose && not (HS.null calls')) $ do
              putStrLn "Learned the values of the following function calls:"
              forM_ calls' (\c -> print . PP.nest 2 $ ppFunctionCallResult env tps (Empty :> Const "self") c)
            let outps = mapMaybe (\(FunctionCallResult fi args _ st) ->
                          if null st then Nothing
                          else Just (ppExpr env tps (Empty :> Const "self")
                                            (ApplyExpr fi args), st)) calls
            when (verbose && not (null outps)) $ do
              putStrLn "The following function calls generated output:"
              forM_ outps (\(d,st) -> print . PP.nest 2 $ d PP.<> PP.colon PP.<+> PP.text st)
            whenANSI $ putStrLn "\n" >> cursorUp 1
            putStrNow "Press enter to see the next instance."
            _ <- getLine
            clearLinesWithCursor 2
            whenNoANSI $ putStrLn ""
            pure True
          | otherwise = pure True
        onInst _ _ 0 ivis = do
          putStrLn $ "Found no valid instances! (Generated "
                     ++ show ivis ++ " invalid instances)"
          pure False
        onInst _ _ vis ivis = do
          putStrLn $ "Enumerated all " ++ show vis ++ " valid instances, "
                     ++ "generated " ++ show ivis ++ " invalid instances"
          pure False
        onLimit :: Natural -> Natural -> IO Bool
        onLimit vis ivis = do
          putStrLn $ "Hit instance limit of " ++ show limit ++ "!"
          putStrLn $ "Found " ++ show vis
                     ++ " valid instances, generated " ++ show ivis
                     ++ " invalid instances"
          whenANSI $ putStrLn "\n" >> cursorUp 1
          putStrNow $ "Press enter to continue enumerating up to "
                      ++ show limit ++ " more instances."
          _ <- getLine
          clearLinesWithCursor 2
          whenNoANSI $ putStrLn ""
          pure True

-- | Given a kind name, an instance limit, and a solver session, this function
-- counts all instances of the given kind using 'generateInstances'.
generateKindInstances :: Text -> Natural -> SessionData env (EmptyCtx ::> tp)
                      -> IO ([Assignment Literal (EmptyCtx ::> tp)], Natural)
generateKindInstances k limit =
  generateInstances msg limit onLimit onInst
  where msg = "Generating instances of '" ++ T.unpack k ++ "'..."
        onInst :: InstanceResult env (EmptyCtx ::> tp)
               -> HS.HashSet (FunctionCallResult env ctx)
               -> Natural -> Natural -> IO Bool
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
        onLimit :: Natural -> Natural -> IO Bool
        onLimit vis ivis = do
          putStrLn $ "Hit instance limit of " ++ show limit ++ "!"
          putStrLn $ "Found " ++ show vis
                     ++ " valid instances, generated " ++ show ivis
                     ++ " invalid instances"
          pure False

-- | Given a check name, a list of names for the fields of the check, an
-- instance limit, and a solver session, this function tries to find a
-- instance of the check (i.e. a counterexample) using 'generateInstances'.
runCheck :: Text -> [Text] -> Natural -> SessionData env ctx
         -> IO (Maybe (Assignment Literal ctx))
runCheck ck fldnms limit s = do
  (ls,_) <- generateInstances msg limit onLimit onInst s
  pure $ listToMaybe ls
  where msg = "Generating counterexamples of '" ++ T.unpack ck ++ "'..."
        onInst :: InstanceResult env ctx
               -> HS.HashSet (FunctionCallResult env ctx)
               -> Natural -> Natural -> IO Bool
        onInst (ValidInstance ls _) _ _ _ = do
          putStrLn $ "'" ++ T.unpack ck ++ "' failed with counterexample:"
          forM_ (zip fldnms (toListFC Some ls)) $ \(fldnm, Some l) ->
            putStrLn $ "  " ++ T.unpack fldnm ++ " = " ++ show (ppLiteral l)
          pure False
        onInst (InvalidInstance _ _ _ _) _ _ _ = pure True
        onInst _ _ vis ivis = do
          putStrLn $ "'" ++ T.unpack ck ++ "' holds. "
                     ++ "(Discarded " ++ show (vis+ivis)
                     ++ " potential counterexamples)"
          pure False
        onLimit :: Natural -> Natural -> IO Bool
        onLimit vis ivis = do
          putStrLn $ "'" ++ T.unpack ck ++ "' holds for the first "
                     ++ show (vis+ivis) ++ " generated instances."
          pure False

-- | Within a solver session, repeatedly asks the solver for instances,
-- calling the given functions as described below for whether or not to
-- continue generation. While generating instances, this function also prints
-- a running count of valid/invalid instances, if ANSI features are enabled.
generateInstances :: forall env ctx.
                     String
                     -- ^ What to print while generating instances. Typically
                     -- something like "Generating instances..."
                  -> Natural
                     -- ^ The limit on the number of instaces to generate. If
                     -- this limit is reached, the following argument is called
                  -> (Natural -> Natural -> IO Bool)
                     -- ^ The function to call if the instance limit is hit.
                     -- Instance generation continues (until the limit is hit
                     -- again) if and only if the returned boolean is true.
                     -- The 'Natural' arguments given are the number of valid
                     -- and invalid instances generated so far, respectively.
                  -> (InstanceResult env ctx -> HS.HashSet (FunctionCallResult env ctx)
                                             -> Natural -> Natural -> IO Bool)
                     -- ^ The function to call each time the solver generates,
                     -- or fails to generate, an instance. Instance generation
                     -- continues if and only if the returned boolean is true, 
                     -- unless the 'InstanceResult' argument given is
                     -- 'NoInstance' or 'Unknown', in which case it always
                     -- stops. The list argument given is the list of all new
                     -- function call results learned, and the @Natural@
                     -- arguments given are the number of valid and invalid
                     -- instances generated so far, respectively.
                  -> SessionData env ctx
                  -> IO ([Assignment Literal ctx], Natural)
generateInstances genMsg limit onLimit onInst s = do
  putStrNow genMsg
  whenNoANSI $ putStrLn ""
  go HS.empty limit 0 0
  where go :: HS.HashSet (FunctionCallResult env ctx)
           -> Natural -> Natural -> Natural
           -> IO ([Assignment Literal ctx], Natural)
        go call_set limit' vis ivis
          | vis + ivis >= limit' = do
          clearLinesWithCursor 1
          cont <- onLimit vis ivis
          if cont then go call_set (limit' + limit) vis ivis
                  else pure ([], vis+ivis)
          | otherwise = do
          getNextInstance s >>= \case
            HasInstance ls fcns calls -> do
              let (toAdd, vis', ivis') = case null fcns of
                    True  -> ([ls], vis+1, ivis) -- ls is a valid instance
                    False -> ([],   vis, ivis+1) -- ls is an invalid instance
              -- only pass calls we haven't seen before to onInst
              let calls' = HS.fromList calls `HS.difference` call_set
              clearLinesWithCursor 1
              cont <- onInst (HasInstance ls fcns calls) calls' vis' ivis'
              if cont then do
                whenANSI $ putStrNow (msg vis' ivis')
                let call_set' = calls' `HS.union` call_set
                (lss, tot) <- go call_set' limit' vis' ivis'
                return (toAdd ++ lss, tot)
              else return (toAdd, vis'+ivis')
            ir -> do
              clearLinesWithCursor 1
              _ <- onInst ir HS.empty vis ivis
              return ([], vis+ivis)
        msg :: Natural -> Natural -> String
        msg 0 0      = genMsg
        msg vis ivis = genMsg ++ " | Found " ++ show vis
                       ++ " valid instances, " ++ show ivis
                       ++ " invalid instances"


-- | The canonical function environment uses 'run' as the implementation for
-- every function in scope
canonicalEnv :: Assignment FunctionTypeRepr fntps
             -> Assignment (FunctionImpl IO) fntps
canonicalEnv Empty = Empty
canonicalEnv (fntps :> fntp@FunctionTypeRepr{}) =
  canonicalEnv fntps :> FunctionImpl fntp (run fntp)

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
