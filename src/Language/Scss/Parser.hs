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
        continueHash name parseName
   in Selector <$> (Text.strip <$> parseName) <*> nestedValues

atRule :: Parser Value
atRule = do
  let parseName = do
        name <- Parser.takeWhileP (Just "an @rule name") $
          \c -> c /= '{' && c /= '#' && c /= '"' && c /= ';'
        asum
          [ stringValue name parseName,
            continueHash name parseName
          ]
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
  let parseName = do
        name <- Parser.takeWhileP (Just "a variable name") $
          \c -> c /= ':' && c /= '#'
        continueHash name parseName <* colon
   in Variable <$> (dollar *> (Text.strip <$> parseName)) <*> propertyValue

property :: Parser Value
property =
  let parseName = do
        name <- Parser.takeWhileP (Just "a property name like display") $
          \c -> c /= ':' && c /= '{' && c /= '#'
        continueHash name parseName
   in Prop <$> (parseName <* colon) <*> (propertyValue <* Parser.notFollowedBy curlyOpen)

propertyValue :: Parser Text
propertyValue = do
  let parseVal = do
        val <- Parser.takeWhileP (Just "a property value like 1px") $
          \c -> c /= ';' && c /= '#' && c /= '{' && c /= '}'
        ifDataUrl val <|> continueHash val parseVal
  value <- lexe parseVal
  semicolon <|> Parser.lookAhead curlyClose
  pure $ Text.strip value

-- HELPERS

continueHash :: Text -> Parser Text -> Parser Text
continueHash val p =
  let hashVar = do
        (\a b c -> a <> Text.strip b <> c)
          <$> Parser.chunk "#{"
          <*> Parser.takeWhileP (Just "some interpolated value") (/= '}')
          <*> Parser.chunk "}"
   in asum
        [ do
            hash <- hashVar <|> Parser.chunk "#"
            mappend (val <> hash) <$> p,
          pure val
        ]

stringValue :: Text -> Parser Text -> Parser Text
stringValue val p =
  let stringVal = do
        (\a b c -> a <> Text.strip b <> c)
          <$> Parser.chunk "\""
          <*> Parser.takeWhileP (Just "some string value") (/= '"')
          <*> Parser.chunk "\""
   in do
        s <- stringVal <|> Parser.chunk "\""
        mappend (val <> s) <$> p

ifDataUrl :: Text -> Parser Text
ifDataUrl val = do
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
