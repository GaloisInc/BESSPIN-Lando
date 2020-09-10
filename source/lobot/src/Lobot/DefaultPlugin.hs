{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Lobot.DefaultPlugin
Description : Definition of the default Lobot plugin.
Copyright   : (c) Matthew Yacavone, Ben Selfridge, 2020
License     : BSD3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : POSIX

This module defines the default Lobot plugin, which for each function in scope,
simply calls an executable which is the name of that function, passing to it
and reading from it JSON from 'Lobot.JSON'.
-}
module Lobot.DefaultPlugin
  ( defaultPluginEnv
  , defaultPluginEnvOnPWD
  ) where

import Lobot.Expr
import Lobot.JSON
import Lobot.Types

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T

import Data.Parameterized.BoolRepr
import Data.Parameterized.Context hiding (null)
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC

import System.Process hiding (env)
import System.FilePath ((</>))
import System.Exit

-- | The environment of function implementations for the default Lobot plugin,
-- parameterized by a directory in which to look for executables.
defaultPluginEnv :: FilePath
                 -> Assignment FunctionTypeRepr fntps
                 -> Assignment (FunctionImpl IO) fntps
defaultPluginEnv _ Empty = Empty
defaultPluginEnv prefix (fntps :> fntp@FunctionTypeRepr{}) =
  defaultPluginEnv prefix fntps :> FunctionImpl fntp (defaultPluginRun prefix fntp)

-- | The environment of function implementations for the default Lobot plugin,
-- using the current working directory.
defaultPluginEnvOnPWD :: Assignment FunctionTypeRepr fntps
                      -> Assignment (FunctionImpl IO) fntps
defaultPluginEnvOnPWD = defaultPluginEnv []

-- | The function used for 'fnImplRun' in the default Lobot plugin. This
-- function calls an executable with the name of the given function on the
-- given path, passes it JSON via stdin, reads JSON from stdout, and returns
-- the parsed JSON and the contents of stderr.
defaultPluginRun :: FilePath
                 -> FunctionTypeRepr (FunType nm args ret)
                 -> Assignment Literal args -> IO (Literal ret, String)
defaultPluginRun prefix FunctionTypeRepr{..} args = do
  let json_args = A.toJSONList (toListFC literalToJSON args)
      std_in = BS.unpack (A.encode json_args)
      p = shell (prefix </> T.unpack (symbolRepr functionName))
  (ec, std_out, std_err) <- readCreateProcessWithExitCode p std_in
  case ec of
    ExitSuccess -> case A.eitherDecode (BS.pack std_out) of
      Right v -> case literalFromJSON v of
        A.Success (Some l') -> case testEquality functionRetType (literalType l') of
          Just Refl -> return (l', std_err)
          Nothing -> do putStrLn $ "expected " ++ show functionRetType
                                   ++ ", got " ++ show l'
                        exitFailure
        A.Error e -> do putStrLn $ "error: " ++ e
                        exitFailure
      Left e -> do putStrLn "Error decoding JSON"
                   putStrLn std_out
                   putStrLn e
                   exitFailure
    ExitFailure _ -> do putStrLn $ "error: " ++ std_err
                        exitFailure
