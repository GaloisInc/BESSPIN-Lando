{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Lobot.Core.Instances
import Lobot.Core.Kind.Pretty
import Lobot.Core.Parser
import Lobot.Core.TypeCheck

import Control.Monad (void)
import Data.Foldable (forM_)
import Data.IORef
import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (last, take)
import Data.Parameterized.Some
import Options.Applicative

data Options = Options { inFileName :: String }
  deriving Show

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
          insts <- collectInstances "/usr/local/bin/z3" Empty k 2000
          let numInsts = length insts
          iRef <- newIORef @Integer 1
          forM_ insts $ \inst -> do
            i <- readIORef iRef
            modifyIORef iRef (+1)
            putStrLn $ "Instance " ++ show i ++ "/" ++ show numInsts ++ ":"
            print $ ppLiteral inst
            putStrLn $ "Press enter to see the next instance."
            void getLine
