{-# LANGUAGE NamedFieldPuns #-}
module Main
  ( main
  , dev
  ) where

import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.Scss.Format as Format
import qualified Language.Scss.Parser as Parser
import Options.Applicative as Options
import Prelude hiding (log)
import System.Exit
import System.IO
import qualified Text.Pretty.Simple as Print


-- MAIN


main :: IO ()
main = do
  args@Args{_aSource, _aVerbose, _aOverwrite, _aVerify} <- parseOptions
  when _aVerbose (logP "Command line arguments" args)
  input <-
    case _aSource of
      FilePath path -> Text.readFile path
      StdIn -> Text.getContents
  when _aVerbose (log "Input" (Text.unpack input))
  case Parser.parse input of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right result -> do
      let formatted = Format.format result
      when _aVerbose (logP "Parse result" result)
      when _aVerify (log "Is formatted" (show (input == formatted)))
      case (_aSource, _aOverwrite) of
        (FilePath path, True) ->
          Text.writeFile path formatted
        _ ->
          Text.putStrLn formatted
  where
    log title x = do
      putStrLn $ "-- " <> title
      putStrLn x
    logP title x = do
      putStrLn $ "-- " <> title
      Print.pPrintNoColor x


-- ARGS


data Source
  = FilePath String
  | StdIn
  deriving (Show)


data Args =
  Args
  { _aVerbose :: !Bool
  , _aOverwrite :: !Bool
  , _aVerify :: !Bool
  , _aSource :: !Source
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
  <*> parseSource


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


parseSource :: Parser Source
parseSource =
  (StdIn <$ parseStdIn) <|> (FilePath <$> parseFilepath)


parseFilepath :: Parser String
parseFilepath =
  strOption $
  long "path"
  <> short 'p'
  <> metavar "PATH"
  <> help "Path to a scss file"


parseStdIn :: Parser Bool
parseStdIn =
  switch $
  long "stdin"
  <> help "Read from stdin"


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
