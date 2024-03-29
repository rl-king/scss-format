{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
    dev,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Either (lefts)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)
import qualified Language.Scss.Format as Format
import qualified Language.Scss.Parser as Parser
import Options.Applicative as Options
import System.Console.ANSI as Console
import qualified System.Directory as Directory
import System.Exit
import qualified System.FilePath.Glob as Glob
import System.FilePath.Posix ((</>))
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
      fileNames <- concat <$> traverse findFiles path
      for_ fileNames $ \fileName -> do
        input <- fmap Format.format . parse fileName =<< Text.readFile fileName
        when verbose $ log "Parse result" input
        if overwrite
          then Text.writeFile fileName input
          else Text.putStrLn input
    Verify path -> do
      fileNames <- concat <$> traverse findFiles path
      files <- for fileNames $ \fileName ->
        (,) (Text.pack fileName) <$> Text.readFile fileName
      result <- Async.forConcurrently files $ \(fileName, input) ->
        pure $
          first (("Parse error in: " <> fileName <> " ") <>) (Parser.parse input)
            >>= verify fileName input . Format.format
      for_ result $ \case
        Right ok -> prettyPrint Console.Green ok
        Left err -> prettyPrint Console.Red err
      if null (lefts result)
        then exitSuccess
        else exitFailure

prettyPrint :: Color -> Text -> IO ()
prettyPrint color message = do
  Console.hSetSGR
    stderr
    [Console.SetColor Console.Foreground Console.Dull color]
  Text.hPutStrLn stderr message
  Console.setSGR [Reset]

verify :: Text -> Text -> Text -> Either Text Text
verify p i f
  | Text.strip i /= f = Left ("✗ " <> p)
  | otherwise = Right ("✓ " <> p)

parse :: String -> Text -> IO [Parser.Value]
parse filename =
  either
    ( \err ->
        hPutStrLn stderr ("Parse error in: " <> filename)
          >> hPutStrLn stderr (Text.unpack err)
          >> exitFailure
    )
    pure
    . Parser.parse

log :: String -> Text -> IO ()
log title x = do
  putStrLn $ "-- " <> title
  Print.pPrintNoColor x

findFiles :: String -> IO [FilePath]
findFiles s =
  let collect =
        (<>)
          <$> Glob.glob (s </> "**/*.scss")
          <*> Glob.glob (s </> "**/*.css")
   in do
        isDir <- Directory.doesDirectoryExist s
        if isDir then collect else pure [s]

-- ARGS

data Args
  = FilePath [String] Overwrite
  | Verify [String]
  | StdIn
  deriving (Show)

type Overwrite = Bool

type Verify = Bool

type Verbose = Bool

parseOptions :: IO (Verbose, Args)
parseOptions =
  let descr =
        concat
          [ "Format scss files and directories, ",
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
   in pick <$> many parsePath <*> parseVerify <*> parseOverwrite
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
      <> help "Path or dir to a scss file(s)"

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
      Text.putStrLn e
    Right r -> do
      putStrLn "\n======\n"
      Text.putStrLn (Format.format r)
      putStrLn "\n======\n"
      Print.pPrint r
