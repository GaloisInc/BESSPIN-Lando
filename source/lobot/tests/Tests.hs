{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Lobot.Expr
import Lobot.Instances hiding (env)
import Lobot.Kind
import Lobot.Parser
import Lobot.TypeCheck
import Lobot.Types
import Lobot.DefaultPlugin

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Parameterized.BoolRepr
import Data.Parameterized.Context hiding (last)
import Data.Parameterized.Some
import Data.Traversable (forM)
import Numeric.Natural
import Control.Monad (when)
import System.Directory
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Text.Show.Pretty (ppShow)

countLimit :: Natural
countLimit = 1000

z3 :: String
z3 = "/usr/local/bin/z3"

main :: IO ()
main = do
  defaultMain =<< goldenTests
  clearTmpDir False

data TestResult where
  TestResult :: { lastKind :: Some (Kind env)
                , instances :: [Some (Assignment Literal)]
                } -> TestResult

deriving instance Show TestResult

-- If `test/tmp` exists, delete it and its contents, and if the given boolean
-- is true, re-create it.
clearTmpDir :: Bool -> IO ()
clearTmpDir recreate = do
  createDirectoryIfMissing False "./tests/tmp"
  removeDirectoryRecursive       "./tests/tmp/"
  when recreate (createDirectory "./tests/tmp/")

testLobotFile :: FilePath -> IO TestResult
testLobotFile fileName = do
  clearTmpDir True
  fileStr <- readFile fileName
  case parseDecls fileName fileStr of
    Left err -> do putStrLn err
                   exitFailure
    Right decls -> case typeCheck decls of
      Left err -> do print err
                     exitFailure
      Right (TypeCheckResult _ [] _, []) -> do
        putStrLn "No kinds in file"
        exitFailure
      Right (TypeCheckResult env ks _, []) -> case last ks of
        Some k -> case isNonAbstract (kindType k) of
          Just IsNonAbs -> do
            (insts, _) <- runSession z3 env (defaultPluginEnv "./tests/functions" env)
                                     (Empty :> kindType k) (kindConstraints k)
                                     (collectAndFilterInstances countLimit)
            return $ TestResult (Some k) (Some <$> insts)
          Nothing -> do
            putStrLn $ "Bad test " ++ fileName ++ ", last kind is abstract"
            exitFailure
      Right (_, ws) -> do
        putStrLn $ "Bad test " ++ fileName ++ ", generated warnings:"
        _ <- forM ws $ \w -> print $ ppTypeWarning fileName w
        exitFailure

goldenTests :: IO TestTree
goldenTests = do
  testFiles <- findByExtension [".lobot"] "./tests/lobot-files"
  return $ testGroup "Golden tests"
    [ goldenVsString
      (takeBaseName testFile)
      resultFile
      (LBS.pack . ppShow <$> testLobotFile testFile)
    | testFile <- testFiles
    , let resultFile = replaceExtension testFile ".result"
    ]
