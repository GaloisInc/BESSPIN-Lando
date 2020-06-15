{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Lobot.Core.Instances
import Lobot.Core.Kind
import Lobot.Core.Kind.JSON
import Lobot.Core.Kind.Pretty
import Lobot.Core.Parser
import Lobot.Core.TypeCheck

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Monad (void, when, filterM)
import Data.Foldable (forM_)
import Data.IORef
import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (last, take)
import Data.Parameterized.Some
import Numeric.Natural
import Options.Applicative
import System.Exit
import System.Process

data Options = Options { inFileName :: String }
  deriving Show

count :: Natural
count = 500

options :: Parser Options
options = Options
      <$> argument str (metavar "FILE")

main :: IO ()
main = ig =<< execParser i
  where i = info (options <**> helper)
          ( fullDesc
         <> progDesc "Generate instances from a Lobot file"
          )

ig :: Options -> IO ()
ig Options{..} = do
  fileStr <- readFile inFileName
  case parseDecls inFileName fileStr of
    Left err -> putStrLn err
    Right decls -> case typeCheck knownRepr decls of
      Left err -> do putStrLn $ "Type error."
                     print err
      Right [] -> print "No kinds in file"
      Right ks -> case last ks of
        Some k -> do
          putStrLn $
            "Last kind in " ++ inFileName ++ ":"
          putStrLn $ "----------------"
          print $ ppKind k
          putStrLn $ "----------------"
          putStrLn $ "Generating instances..."
          insts <- collectInstances "/usr/local/bin/z3" knownRepr k count
          putStrLn $ "Generated " ++ show (length insts) ++ " instances."
          putStrLn $ "Filtering for valid instances..."
          validInsts <- flip filterM insts $ \inst -> instanceOf fnEnv inst k
          putStrLn $ show (length validInsts) ++ " valid instances."
          let numInsts = length validInsts
          iRef <- newIORef 1
          forM_ validInsts $ \inst -> do
            i <- readIORef iRef
            modifyIORef iRef (+1)
            putStrLn $
              "Instance " ++ show i ++ "/" ++ show numInsts ++ ":"
            print $ ppLiteral inst
            when (i < numInsts) $ do
              putStrLn $ "Press enter to see the next instance."
              void getLine

type FnEnv = EmptyCtx ::> FunType "add1" (EmptyCtx ::> IntType) IntType

add1 :: Assignment Literal (EmptyCtx ::> IntType) -> IO (Literal IntType)
add1 (Empty :> l) = do
  let std_in = BS.unpack (A.encode (literalToJSON l))
      p = shell "add1"
  (ec, std_out, std_err) <- readCreateProcessWithExitCode p std_in
  case ec of
    ExitSuccess -> case A.eitherDecode (BS.pack std_out) of
      Right v -> case literalFromJSON v of
        A.Success (Some l') -> case testEquality IntRepr (literalType l') of
          Just Refl -> return l'
          Nothing -> do putStrLn $ "expected int, got " ++ show l'
                        exitFailure
        A.Error e -> do putStrLn $ "error: " ++ e
                        exitFailure
      Left e -> do putStrLn "Error decoding JSON"
                   putStrLn std_out
                   putStrLn e
                   exitFailure
    ExitFailure _ -> do putStrLn $ "error: " ++ std_err
                        exitFailure

fnEnv :: Assignment (FunctionImpl IO) FnEnv
fnEnv = Empty :> FunctionImpl knownRepr add1
