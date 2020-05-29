{-# LANGUAGE OverloadedStrings #-}

module Language.Scss.FormatSpec where

import qualified Data.Text as Text
import Data.Text.IO as Text
import Language.Scss.Format
import Language.Scss.Parser
import System.Directory
import System.FilePath.Posix
import Test.Hspec

spec :: Spec
spec = do
  files <-
    runIO $
      mapM getFiles
        =<< filter ((==) ".scss" . takeExtension)
        <$> listDirectory unformatted
  mapM_ test files
  where
    formatted =
      "examples/formatted/"
    unformatted =
      "examples/"
    getFiles name =
      (,,)
        <$> Text.readFile (unformatted ++ name)
        <*> Text.readFile (formatted ++ name)
        <*> pure name
    test (unfo, forma, name) =
      it (name ++ " parse and format") $
        format <$> parse unfo `shouldBe` Right (Text.strip forma)
