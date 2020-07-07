{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

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
import qualified Data.Text as T

import Control.Monad (void, when)
import Data.Maybe
import Data.Foldable (forM_)
import Data.IORef
import Data.Parameterized.BoolRepr
import Data.Parameterized.Context hiding (last, take)
import Data.Parameterized.Some
import Data.Parameterized.SymbolRepr
import Data.Parameterized.TraversableFC
import Numeric.Natural
import Options.Applicative
import System.Directory
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
      Right (TypeCheckResult env _ cks, ws) -> do
        forM_ ws $ print . ppTypeWarning inFileName
        forM_ cks $ \(Some ck) -> do
          let tps = fmapFC namedTypeType (checkFields ck)
          () <- case anyAbstractTypes tps of
            FalseRepr -> do
              putStrLn $ "Checking " ++ T.unpack (checkName ck) ++ "..."
              let constraints = checkConstraints ck ++ [negRequirements]
                  negRequirements = NotExpr (foldr AndExpr (LiteralExpr (BoolLit True)) (checkRequirements ck))
              (failedInsts, totalInsts) <-
                collectAndFilterInstances z3 env (canonicalEnv env) tps constraints count
              case failedInsts of
                [] -> putStrLn $ T.unpack (checkName ck) ++ " holds!"
                (i:_) -> do
                  putStrLn $ "Found " ++ show (length failedInsts) ++ " failing instances."
                  putStrLn $ "Example: "
                  forM_ (zip (toListFC namedTypeName (checkFields ck)) (toListFC Some i)) $ \(nm, Some l) ->
                            putStrLn $ T.unpack nm ++ " = " ++ show (ppLiteral l)
            TrueRepr -> do
              putStrLn $ "Cannot check properties of abstract types."
              exitFailure
          return ()
      -- Right (TypeCheckResult env ks [], ws) -> case last ks of
      --   Some k -> do
      --     forM_ ws $ print . ppTypeWarning inFileName
      --     putStrLn $
      --       "Last kind in " ++ inFileName ++ ":"
      --     putStrLn $ "----------------"
      --     print $ ppKind k
      --     putStrLn $ "----------------"
      --     case isAbstractType (kindType k) of
      --       FalseRepr -> do
      --         putStrLn $ "Generating instances..."
      --         (validInsts, totalInsts) <-
      --           collectAndFilterInstances z3 env (canonicalEnv env) (Empty :> kindType k) (kindConstraints k) count
      --         putStrLn $ show (length validInsts) ++ " valid instances, enumerated " ++ show totalInsts
      --         let numInsts = length validInsts
      --         iRef <- newIORef 1
      --         forM_ validInsts $ \(Empty :> inst) -> do
      --           i <- readIORef iRef
      --           modifyIORef iRef (+1)
      --           putStrLn $
      --             "Instance " ++ show i ++ "/" ++ show numInsts ++ ":"
      --           print $ ppLiteralWithKindName (kindName k) inst
      --           when (i < numInsts) $ do
      --             putStrLn $ "Press enter to see the next instance."
      --             void getLine
      --       TrueRepr -> do
      --         putStrLn $ "Cannot generate instances of abstract type."
      --         exitFailure

canonicalEnv :: Assignment FunctionTypeRepr fntps
             -> Assignment (FunctionImpl IO) fntps
canonicalEnv Empty = Empty
canonicalEnv (fntps :> fntp@FunctionTypeRepr{}) = canonicalEnv fntps :> canonicalFn fntp

canonicalFn :: FunctionTypeRepr (FunType nm args ret)
            -> FunctionImpl IO (FunType nm args ret)
canonicalFn fntp = FunctionImpl fntp (run fntp)

run :: FunctionTypeRepr (FunType nm args ret)
    -> Assignment Literal args
    -> IO (Literal ret)
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
