{-# LANGUAGE OverloadedStrings #-}
module Language.Scss.Parser
  ( parse
  , parser
  , Parser
  , Value(..)
  ) where

import Control.Applicative hiding (many)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Text.Megaparsec as Parser
import Text.Megaparsec.Char


type Parser a =
  Parser.Parsec Void Text a


data Value
  = Selector Text [Value]
  | AtRule Text Text [Value]
  | Prop Text Text
  deriving (Show)


parse :: Text -> Either String [Value]
parse =
  first Parser.errorBundlePretty . Parser.runParser parser ""


parser :: Parser [Value]
parser =
  Parser.manyTill (atRule <|> selector) Parser.eof


selector :: Parser Value
selector = do
  name <- Parser.takeWhileP (Just "a selector") (/= '{')
  surround whitespace curlyOpen
  ps <- Parser.manyTill (Parser.try prop <|> atRule <|> selector) (char '}')
  whitespace
  pure $ Selector (Text.strip name) ps


atRule :: Parser Value
atRule = do
  _ <- char '@'
  rule <- Parser.takeWhileP (Just "a rule") (/= ' ')
  name <- Parser.takeWhileP (Just "a rule name") (\t -> t /= ';' && t /= '{')
  maybeSemi <- optional semicolon
  case maybeSemi of
    Just _ -> do
      whitespace
      pure $ AtRule rule (Text.strip name) []
    Nothing -> do
      surround whitespace curlyOpen
      ps <- Parser.manyTill (Parser.try prop <|> selector) (char '}')
      whitespace
      pure $ AtRule rule (Text.strip name) ps


prop :: Parser Value
prop =
  Prop <$> propName <*> propVal


propName :: Parser Text
propName = do
  maybeCombinator <- optional (Parser.oneOf ['&', '>', '~', '+'])
  case maybeCombinator of
    Just _ ->
      empty
    Nothing ->
      Parser.takeWhileP (Just "a prop name") (\t -> t /= ':' && t /= ' ')
      <* surround whitespace colon



propVal :: Parser Text
propVal = do
  v <- Parser.takeWhileP (Just "a prop value")
    (\t -> t /= '#' && t /= '}' && t /= ';')
  maybeHashVar <- optional hashVar
  case maybeHashVar of
    Nothing -> do
      surround whitespace (semicolon <|> Parser.lookAhead curlyClose)
      pure (Text.strip v)
    Just hashVar' -> do
      v2 <- propVal
      pure (Text.strip v <> hashVar' <> Text.strip v2)


hashVar :: Parser Text
hashVar =
  (\a b c -> a <> b <> c)
  <$> string "#{"
  <*> Parser.takeWhileP (Just "a hash var") (/= '}')
  <*> string "}"


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


surround :: Parser a -> Parser b -> Parser b
surround a b =
  a *> b <* a
