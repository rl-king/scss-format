{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.Text.IO as Text
import Language.Scss.Format
import Language.Scss.Parser

main :: IO ()
main = do
  file <- Text.readFile "style.scss"
  let (Right parsed) = parse file
  defaultMain
    [ bench "parse file" $ whnf parse file,
      bench "format file" $ whnf format parsed
    ]
