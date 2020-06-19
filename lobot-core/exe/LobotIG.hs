{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Lobot.Instances
import Lobot.Kind
import Lobot.Kind.JSON
import Lobot.Kind.Pretty
import Lobot.Parser
import Lobot.TypeCheck
import Lobot.Types

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T

import Control.Monad (void, when, filterM)
import Data.Foldable (forM_)
import Data.IORef
import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (last, take)
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Numeric.Natural
import Options.Applicative
import System.Exit
import System.Process

data Options = Options { inFileName :: String }
  deriving Show

count :: Natural
count = 100

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
      Left err -> print $ ppTypeError inFileName err
      Right [] -> print "No kinds in file"
      Right ks -> case last ks of
        Some k -> do
          putStrLn $
            "Last kind in " ++ inFileName ++ ":"
          putStrLn $ "----------------"
          print $ ppKind k
          putStrLn $ "----------------"
          putStrLn $ "Generating instances..."
          mInsts <- collectInstances "/usr/local/bin/z3" knownRepr k count
          when (not (isJust mInsts)) $ do
            putStrLn $ "Can't generate instances of abstract type " ++ T.unpack (kindName k)
            exitFailure
          let Just insts = mInsts
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

type FnEnv = EmptyCtx ::>
  FunType "add1" (EmptyCtx ::> IntType) IntType ::>
  FunType "square" (EmptyCtx ::> IntType) IntType ::>
  FunType "double" (EmptyCtx ::> IntType) IntType

fnEnv :: Assignment (FunctionImpl IO) FnEnv
fnEnv = canonicalEnv knownRepr

canonicalEnv :: Assignment FunctionTypeRepr fntps
             -> Assignment (FunctionImpl IO) fntps
canonicalEnv Empty = Empty
canonicalEnv (fntps :> fntp@FunctionTypeRepr{}) = canonicalEnv fntps :> canonicalFn fntp

canonicalFn :: FunctionTypeRepr (FunType nm args ret)
            -> FunctionImpl IO (FunType nm args ret)
canonicalFn fntp = FunctionImpl fntp (run fntp)

run :: FunctionTypeRepr (FunType nm args ret) -> Assignment Literal args -> IO (Literal ret)
run FunctionTypeRepr{..} args = do
  let json_args = A.toJSONList (toListFC literalToJSON args)
      std_in = BS.unpack (A.encode json_args)
      p = shell (T.unpack (symbolRepr functionName))
  (ec, std_out, std_err) <- readCreateProcessWithExitCode p std_in
  case ec of
    ExitSuccess -> case A.eitherDecode (BS.pack std_out) of
      Right v -> case literalFromJSON v of
        A.Success (Some l') -> case testEquality functionRetType (literalType l') of
          Just Refl -> return l'
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
