{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Control.Applicative hiding (many)
import Data.Text as Text
import Data.Text.IO as Text
import Data.Void
import qualified Text.Megaparsec as Parser
import Text.Megaparsec.Char
import qualified Text.Pretty.Simple as Print


-- DEFINITIONS


type Parser a =
  Parser.Parsec Void Text a


data Value
  = Selector Text [Value]
  | AtRule Text Text [Value]
  | Prop Text Text
  deriving (Show)


-- RUN


run :: IO ()
run = do
  stylesheet <- Text.readFile "style.scss"
  print stylesheet
  Print.pPrint $ Parser.runParser parser "" stylesheet


-- PARSER


parser :: Parser [Value]
parser =
  Parser.manyTill (atRule <|> selector) Parser.eof


selector :: Parser Value
selector = do
  name <- Parser.takeWhileP (Just "selector") (\t -> t /= '{')
  whitespace
  curlyOpen
  whitespace
  ps <- Parser.someTill (Parser.try prop <|> atRule <|> selector) (char '}')
  whitespace
  pure $ Selector (strip name) ps


atRule :: Parser Value
atRule = do
  _ <- string "@"
  rule <- Parser.takeWhileP (Just "at rule") (\t -> t /= ' ')
  name <- Parser.takeWhileP (Just "at rule name") (\t -> t /= '{')
  whitespace
  curlyOpen
  whitespace
  ps <- Parser.someTill (Parser.try prop <|> selector) (char '}')
  whitespace
  pure $ AtRule rule (strip name) ps


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
