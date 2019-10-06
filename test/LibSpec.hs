{-# LANGUAGE OverloadedStrings #-}
module LibSpec where

import Data.Text (Text)
import Data.Text.IO as Text
import Lib
import System.Directory
import System.FilePath.Posix
import Test.Hspec


unformatted :: String
unformatted =
  "examples/"


formatted :: String
formatted =
  "examples/formatted/"


spec :: Spec
spec = do
  files <- runIO $ mapM getFiles
    =<< filter ((==) ".scss" . takeExtension)
    <$> listDirectory unformatted
  runIO (print $ length files)
  mapM_ test files
  where
    getFiles name =
      (,,)
      <$> Text.readFile (unformatted ++ name)
      <*> Text.readFile (formatted ++ name)
      <*> pure name


test :: (Text, Text, String) -> Spec
test (unf, for, name) =
  it (name ++ " parse and format") $
  format unf `shouldBe` Right for
