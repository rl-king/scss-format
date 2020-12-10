{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
    dev,
  )
where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.Scss.Format as Format
import qualified Language.Scss.Parser as Parser
import Options.Applicative as Options
import System.Exit
import System.IO
import qualified Text.Pretty.Simple as Print
import Prelude hiding (log)

-- MAIN

main :: IO ()
main = do
  (verbose, args) <- parseOptions
  when verbose $ log "Command line arguments" (Text.pack $ show args)
  case args of
    StdIn ->
      Text.putStrLn . Format.format =<< parse "stdin" =<< Text.getContents
    FilePath path overwrite -> do
      input <- fmap Format.format . parse path =<< Text.readFile path
      when verbose $ log "Parse result" input
      if overwrite
        then Text.writeFile path input
        else Text.putStrLn input
    Verify path -> do
      input <- Text.readFile path
      parsed <- parse path input
      verify (Text.pack path) input (Format.format parsed)

parse :: String -> Text -> IO [Parser.Value]
parse filename =
  either
    ( \err ->
        hPutStrLn stderr ("Parse error in: " <> filename)
          >> hPutStrLn stderr err
          >> exitFailure
    )
    pure
    . Parser.parse

log :: String -> Text -> IO ()
log title x = do
  putStrLn $ "-- " <> title
  Print.pPrintNoColor x

verify :: Text -> Text -> Text -> IO ()
verify p i f
  | Text.strip i /= f = Text.hPutStrLn stderr ("✗ " <> p) >> exitWith (ExitFailure 100)
  | otherwise = Text.hPutStrLn stdout ("✓ " <> p) >> exitSuccess

-- ARGS

data Args
  = FilePath String Overwrite
  | Verify String
  | StdIn
  deriving (Show)

type Overwrite = Bool

type Verify = Bool

type Verbose = Bool

parseOptions :: IO (Verbose, Args)
parseOptions =
  let descr =
        concat
          [ "Format scss files, ",
            "prints the result to stdout by default, ",
            "use '-o' to replace the original file."
          ]
   in customExecParser (prefs showHelpOnError) $
        info
          ((,) <$> parseVerbose <*> parser <**> helper)
          (fullDesc <> progDesc descr)

parser :: Parser Args
parser =
  let pick a b c
        | b = Verify a
        | otherwise = FilePath a c
   in pick <$> parsePath <*> parseVerify <*> parseOverwrite
        <|> StdIn <$ parseStdIn

parseVerbose :: Parser Verbose
parseVerbose =
  switch $
    long "verbose"
      <> showDefault
      <> help "Log a bit"

parseOverwrite :: Parser Overwrite
parseOverwrite =
  switch $
    long "overwrite"
      <> short 'o'
      <> showDefault
      <> help "Replace the orginal file"

parseVerify :: Parser Verify
parseVerify =
  switch $
    long "verify"
      <> short 'v'
      <> showDefault
      <> help "Test if file is correctly formatted"

parsePath :: Parser String
parsePath =
  strOption $
    long "path"
      <> short 'p'
      <> metavar "PATH"
      <> help "Path to a scss file"

parseStdIn :: Parser Bool
parseStdIn =
  switch $
    long "stdin" <> help "Read from stdin"

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
