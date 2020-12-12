{-# LANGUAGE OverloadedStrings #-}

module Language.Scss.Parser
  ( parse,
    parser,
    Parser,
    Value (..),
  )
where

import Control.Applicative hiding (many)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Text.Megaparsec as Parser
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser a =
  Parser.Parsec Void Text a

data Value
  = Selector Text [Value]
  | AtRule Text Text [Value]
  | Prop Text Text
  | Variable Text Text
  | MultilineComment Text
  | Comment Text
  deriving (Show)

parse :: Text -> Either Text [Value]
parse =
  first (Text.pack . Parser.errorBundlePretty) . Parser.runParser parser ""

parser :: Parser [Value]
parser =
  ws *> values Parser.eof

values :: Parser () -> Parser [Value]
values =
  Parser.manyTill $
    multilineComment
      <|> comment
      <|> variable
      <|> atRule
      <|> Parser.try property
      <|> selector

nestedValues :: Parser [Value]
nestedValues =
  lexe $ curlyOpen *> values curlyClose

selector :: Parser Value
selector =
  let parseName = do
        name <- Parser.takeWhileP (Just "a selector name like main, .class or #id") $
          \c -> c /= '{' && c /= '#'
        continueIfHash name parseName
   in Selector <$> (Text.strip <$> parseName) <*> nestedValues

atRule :: Parser Value
atRule = do
  let parseName = do
        name <- Parser.takeWhileP (Just "an @rule name") $
          \c -> c /= '{' && c /= '#' && c /= ';'
        continueIfHash name parseName
  _ <- at
  rule <- Parser.takeWhileP (Just "@ rule") $
    \t -> t /= '{' && t /= ';' && t /= ' '
  name <- parseName
  asum
    [ AtRule rule (Text.strip name) [] <$ Parser.try semicolon,
      AtRule rule (Text.strip name) <$> nestedValues
    ]

variable :: Parser Value
variable =
  Variable <$> (dollar *> propName) <*> propVal

property :: Parser Value
property = do
  let parseName = do
        name <- Parser.takeWhileP (Just "a property name like display") $
          \c -> c /= ':' && c /= '{' && c /= '#'

        continueIfHash name parseName
  Prop <$> (parseName <* colon) <*> (propVal <* Parser.notFollowedBy curlyOpen)

propName :: Parser Text
propName = do
  let parseName = do
        name <- Parser.takeWhileP (Just "a property name like display") $
          \c -> c /= ':' && c /= '#'
        continueIfHash name parseName
  Text.strip <$> (parseName <* colon)

propVal :: Parser Text
propVal = do
  let parseVal = do
        val <- Parser.takeWhileP (Just "a property value like 1px") $
          \c -> c /= ';' && c /= '#' && c /= '{' && c /= '}'
        continueIfDataUrl val <|> continueIfHash val parseVal
  value <- lexe parseVal
  semicolon <|> Parser.lookAhead curlyClose
  pure $ Text.strip value

-- HELPER

continueIfHash :: Text -> Parser Text -> Parser Text
continueIfHash val p =
  let hashVar = do
        (\a b c -> a <> Text.strip b <> c)
          <$> Parser.chunk "#{"
          <*> Parser.takeWhileP (Just "a hash var") (/= '}')
          <*> Parser.chunk "}"
   in asum
        [ do
            hash <- hashVar <|> Parser.chunk "#"
            mappend (val <> hash) <$> p,
          pure val
        ]

continueIfDataUrl :: Text -> Parser Text
continueIfDataUrl val = do
  base <- Parser.chunk ";base64"
  rest <- Parser.takeWhileP (Just "a base64 value") (\t -> t /= '}' && t /= ';')
  pure (Text.stripEnd val <> base <> Text.stripEnd rest)

-- COMMENTS

multilineComment :: Parser Value
multilineComment =
  let parseLine = do
        value <- Parser.takeWhileP (Just "a multiline comment") (/= '*')
        asum
          [ value <$ Parser.try (Parser.chunk "*/"),
            do
              _ <- char '*'
              mappend (value <> "*") <$> parseLine
          ]
   in do
        _ <- Parser.chunk "/*"
        MultilineComment <$> lexe parseLine

comment :: Parser Value
comment = do
  _ <- Parser.chunk "//"
  lexe $ Comment <$> Parser.takeWhileP (Just "a comment") (/= '\n')

semicolon :: Parser ()
semicolon =
  () <$ lexe (char ';')

colon :: Parser ()
colon =
  () <$ lexe (char ':')

curlyOpen :: Parser ()
curlyOpen =
  () <$ lexe (char '{')

curlyClose :: Parser ()
curlyClose =
  () <$ lexe (char '}')

dollar :: Parser ()
dollar =
  () <$ lexe (char '$')

at :: Parser ()
at =
  () <$ lexe (char '@')

lexe :: Parser a -> Parser a
lexe =
  (<* ws)

ws :: Parser ()
ws =
  space <|> void eol <|> void newline
