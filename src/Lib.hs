{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Control.Applicative hiding (many)
import Data.Text
import Data.Void
import qualified Text.Megaparsec as Parser
import Text.Megaparsec.Char


type Parser a =
  Parser.Parsec Void Text a


data Tree
  = Node [(Text, [(Text, Text)])] [Tree]
  | Empty
  deriving (Show)


run :: IO ()
run =
  Parser.parseTest parser " .someFunc { display: block; position: relative;} "


parser :: Parser Tree
parser = do
  space
  s <- selector
  r <- rule
  pure $ Node [(s, r)] []


selector :: Parser Text
selector =
  Parser.takeWhileP (Just "selector") (\t -> t /= '{' && t /= ' ')
  <* space
  <* char '{'
  <* space


rule :: Parser [(Text, Text)]
rule =
  Parser.manyTill ((,) <$> prop <*> val) (Parser.eof <|> () <$ char '}')


prop :: Parser Text
prop =
  Parser.takeWhileP (Just "prop") (\t -> t /= ':' && t /= ' ')
  <* space
  <* colon
  <* space


val :: Parser Text
val =
  Parser.takeWhileP (Just "val") (\t -> t /= '}' && t /= ';')
  <* space
  <* semicolon
  <* space


semicolon :: Parser ()
semicolon =
  () <$ char ';'


colon :: Parser ()
colon =
  () <$ char ':'
