module Main where

import Lobot.Core.Instances
import Lobot.Core.Kind
import Lobot.Core.Parser
import Lobot.Core.TypeCheck

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (last)
import Data.Parameterized.Some
import Numeric.Natural
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Text.Show.Pretty (ppShow)

countLimit :: Natural
countLimit = 2000

z3 :: String
z3 = "/usr/local/bin/z3"

main :: IO ()
main = defaultMain =<< goldenTests

data TestResult = TestResult { lastKind :: Some (Kind EmptyCtx)
                             , instances :: [Some Literal]
                             }
  deriving Show

testLobotFile :: FilePath -> IO TestResult
testLobotFile fileName = do
  fileStr <- readFile fileName
  case parseDecls fileName fileStr of
    Left err -> do putStrLn err
                   exitFailure
    Right decls -> case typeCheck knownRepr decls of
      Left err -> do print err
                     exitFailure
      Right [] -> do print "No kinds in file"
                     exitFailure
      Right ks -> case last ks of
        Some k -> do
          insts <- collectInstances z3 Empty k countLimit
          return $ TestResult (Some k) (Some <$> insts)

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
