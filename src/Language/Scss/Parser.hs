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
  | MultilineComment Text Text
  | Comment Text
  deriving (Show)


parse :: Text -> Either String [Value]
parse =
  first Parser.errorBundlePretty . Parser.runParser parser ""


parser :: Parser [Value]
parser =
  Parser.manyTill
  (multilineComment
   <|> comment
   <|> Parser.try prop
   <|> atRule
   <|> selector
  ) Parser.eof


selector :: Parser Value
selector = do
  name <- Parser.takeWhileP (Just "a selector") (/= '{')
  Selector (Text.strip name) <$> nestedValues


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
    Nothing ->
      AtRule rule (Text.strip name) <$> nestedValues


nestedValues :: Parser [Value]
nestedValues =
  surround whitespace curlyOpen *>
  Parser.manyTill
    (multilineComment
     <|> comment
     <|> Parser.try prop
     <|> atRule
     <|> selector
    ) (char '}')
  <* whitespace


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
  <$> Parser.chunk "#{"
  <*> Parser.takeWhileP (Just "a hash var") (/= '}')
  <*> Parser.chunk "}"


multilineComment :: Parser Value
multilineComment = do
  _ <- Parser.chunk "/*"
  c <- Parser.takeWhileP (Just "a multiline comment") (/= '*')
  closing <- optional (Parser.chunk "*/")
  case closing of
    Nothing ->
      multilineComment
    Just _ -> do
      ws <- Parser.takeWhileP
        (Just "space or newline") (\t -> t == ' ' || t == '\n')
      pure (MultilineComment c ws)


comment :: Parser Value
comment = do
  _ <- Parser.chunk "//"
  c <- Parser.takeWhileP (Just "a comment") (/= '\n')
  whitespace
  pure (Comment c)


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
