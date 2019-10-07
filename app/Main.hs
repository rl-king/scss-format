{-# LANGUAGE NamedFieldPuns #-}
module Main
  ( main
  , dev
  ) where

import Control.Monad (when)
import qualified Data.Text.IO as Text
import qualified Language.Scss.Format as Format
import qualified Language.Scss.Parser as Parser
import Options.Applicative as Options
import Prelude hiding (log)
import qualified Text.Pretty.Simple as Print


-- MAIN


main :: IO ()
main = do
  args@Args{_aFilepath, _aVerbose, _aOverwrite, _aVerify} <- parseOptions
  when _aVerbose (logP "Command line arguments" args)
  input <- Text.readFile _aFilepath
  when _aVerbose (log "Input" input)
  case Parser.parse input of
    Left err ->
      log "Parse error" err
    Right result -> do
      let formatted = Format.format result
      when _aVerbose (logP "Parse result" result)
      if _aVerify
        then log "Is formatted" (input == formatted)
        else if _aOverwrite
             then Text.writeFile _aFilepath formatted
             else Text.putStrLn formatted
  where
    log title x = do
      putStrLn $ "-- " <> title
      print x
    logP title x = do
      putStrLn $ "-- " <> title
      Print.pPrintNoColor x


-- ARGS


data Args =
  Args
  { _aVerbose :: !Bool
  , _aOverwrite :: !Bool
  , _aVerify :: !Bool
  , _aFilepath :: !String
  } deriving (Show)


parseOptions :: IO Args
parseOptions =
  customExecParser (prefs showHelpOnError) $
  info (parser <**> helper) (fullDesc <> progDesc descr)
  where
    descr =
      concat
      [ "Format scss files, "
      , "prints the result to stdout by default, "
      , "use '-o' to replace the original file."
      ]


parser :: Parser Args
parser =
  Args
  <$> parseVerbose
  <*> parseOverwrite
  <*> parseVerify
  <*> parseFilepath


parseVerbose :: Parser Bool
parseVerbose =
  switch $
  long "verbose"
  <> short 'v'
  <> showDefault
  <> help "Log a bit"


parseOverwrite :: Parser Bool
parseOverwrite =
  switch $
  long "overwrite"
  <> short 'o'
  <> showDefault
  <> help "Replace the orginal file"


parseVerify :: Parser Bool
parseVerify =
  switch $
  long "verify"
  <> short 't'
  <> showDefault
  <> help "Test if file is correctly formatted"


parseFilepath :: Parser String
parseFilepath =
  strOption $
  long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> help "Path to a scss file"


-- DEV


dev :: IO ()
dev = do
  input <- Text.readFile "style.scss"
  case Parser.parse input of
    Left e ->
      putStrLn e
    Right r -> do
      putStrLn "\n======\n"
      Text.putStrLn (Format.format r)
      putStrLn "\n======\n"
      Print.pPrint r
