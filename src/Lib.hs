{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Control.Applicative hiding (many)
import Data.Text as Text
import Data.Text.IO as Text
import Data.Void
import qualified Text.Megaparsec as Parser
import Text.Megaparsec.Char


type Parser a =
  Parser.Parsec Void Text a


data Value
  = Selector Text [Value]
  | Prop Text Text
  deriving (Show)


run :: IO ()
run = do
  stylesheet <- Text.readFile "style.scss"
  print stylesheet
  Parser.parseTest parser stylesheet


parser :: Parser [Value]
parser =
  Parser.someTill selector Parser.atEnd


selector :: Parser Value
selector = do
  name <- Parser.takeWhileP (Just "selector") (\t -> t /= '{' && t /= ' ')
  whitespace
  curlyOpen
  whitespace
  ps <- Parser.someTill (Parser.try prop <|> selector) (char '}')
  whitespace
  pure $ Selector name ps


prop :: Parser Value
prop =
  Prop <$> propName <*> propVal


propName :: Parser Text
propName =
  Parser.takeWhileP (Just "prop") (\t -> t /= ':' && t /= ' ')
  <* whitespace
  <* colon
  <* whitespace


propVal :: Parser Text
propVal =
  Parser.takeWhileP (Just "propVal") (\t -> t /= '}' && t /= ';' && t /= '\n')
  <* whitespace
  <* semicolon
  <* whitespace


semicolon :: Parser ()
semicolon =
  () <$ char ';'


colon :: Parser ()
colon =
  () <$ char ':'


curlyOpen :: Parser ()
curlyOpen =
  () <$ char '{'


curlyClose :: Parser ()
curlyClose =
  () <$ char '}'


whitespace :: Parser ()
whitespace =
  space <|> () <$ eol <|> () <$ newline
