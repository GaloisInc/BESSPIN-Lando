module Main where

import Lobot.Core.Instances
import Lobot.Core.Kind
import Lobot.Core.Parser
import Lobot.Core.TypeCheck

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Parameterized.Classes
import Data.Parameterized.Context hiding (last)
import Data.Parameterized.Some
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Text.Show.Pretty (ppShow)

countLimit :: Integer
countLimit = 2000

main :: IO ()
main = defaultMain =<< goldenTests

data TestResult = TestResult { lastKind :: Some (Kind EmptyCtx)
                             , numInstances :: Integer
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
          n <- countInstances countLimit "/usr/local/bin/z3" Empty k
          return $ TestResult (Some k) n

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
