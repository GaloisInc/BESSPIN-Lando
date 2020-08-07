{-# LANGUAGE GADTs #-}

module Main where

import Lobot.Expr
import Lobot.Instances
import Lobot.Kind
import Lobot.Parser
import Lobot.TypeCheck
import Lobot.Types

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Parameterized.BoolRepr
import Data.Parameterized.Context hiding (last)
import Data.Parameterized.Some
import Data.Traversable (forM)
import Data.Constraint (Dict(..))
import Numeric.Natural
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
main = defaultMain =<< goldenTests

data TestResult = TestResult { lastKind :: Some (Kind EmptyCtx)
                             , instances :: [Some (Assignment Literal)]
                             }
  deriving Show

testLobotFile :: FilePath -> IO TestResult
testLobotFile fileName = do
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
      Right (TypeCheckResult Empty ks _, []) -> case last ks of
        Some k -> case isNonAbstract (kindType k) of
          Just Dict -> do
            (insts, _) <- runSession z3 Empty Empty (Empty :> kindType k) (kindConstraints k) (collectAndFilterInstances countLimit)
            return $ TestResult (Some k) (Some <$> insts)
          Nothing -> do
            putStrLn $ "Bad test " ++ fileName ++ ", last kind is abstract"
            exitFailure
      Right (TypeCheckResult _ _ _, []) -> do
        putStrLn $ "Bad test " ++ fileName ++ ", contained function declaration"
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
