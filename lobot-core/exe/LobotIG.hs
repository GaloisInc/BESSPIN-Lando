{-# LANGUAGE RecordWildCards #-}
module Main where

import Lobot.Core.Instances
import Lobot.Core.Kind.Pretty
import Lobot.Core.Lexer
import Lobot.Core.Parser
import Lobot.Core.TypeCheck

import Data.Foldable
import Data.Parameterized.Context hiding (last)
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
  let tokens = lexLobot fileStr
      decls = parse tokens
  case typeCheck Empty decls of
    Left err -> do putStrLn $ "Type error."
                   print err
    Right [] -> print "No kinds in file"
    Right ks -> do
      putStrLn "Press enter to see a new instance."
      case last ks of
        Some k -> instanceSession Empty "/usr/local/bin/z3" Empty k
