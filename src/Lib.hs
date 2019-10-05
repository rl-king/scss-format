{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Control.Applicative hiding (many)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
  case Parser.runParser parser "" stylesheet of
    Left e ->
      putStrLn $ Parser.errorBundlePretty e
    Right r -> do
      putStrLn "\n======\n"
      Text.putStrLn (render r)
      putStrLn "\n======\n"
      Print.pPrint r
      -- Text.writeFile "style.scss" (render r)


render :: [Value] -> Text
render =
  Text.strip . fst . foldr renderValue ("", 0)


renderValue :: Value -> (Text, Int) -> (Text, Int)
renderValue value (acc, i) =
  case value of
    Selector name values ->
      ("\n"
       <> indent i
       <> name
       <> " {\n"
       <> fst (foldr renderValue ("", i + 1) values)
       <> indent i
       <> "}\n"
       <> acc
      , i
      )
    AtRule rule name values ->
      ("\n"
       <> indent i
       <>"@"
       <> rule
       <> " "
       <> name
       <> " {\n"
       <> fst (foldr renderValue ("", i + 1) values)
       <> indent i
       <> "}\n"
       <> acc
      , i
      )
    Prop name v ->
      (indent i
       <> name
       <> ": "
       <> v
       <> ";\n"
       <> acc
      , i
      )
  where
    indent i' =
      Text.replicate i' "    "


-- PARSER


parser :: Parser [Value]
parser =
  Parser.manyTill (atRule <|> selector) Parser.eof


selector :: Parser Value
selector = do
  name <- Parser.takeWhileP (Just "selector") (/= '{')
  surround whitespace curlyOpen
  ps <- Parser.someTill (Parser.try prop <|> atRule <|> selector) (char '}')
  whitespace
  pure $ Selector (Text.strip name) ps


atRule :: Parser Value
atRule = do
  _ <- char '@'
  rule <- Parser.takeWhileP (Just "at rule") (/= ' ')
  name <- Parser.takeWhileP (Just "at rule name") (/= '{')
  surround whitespace curlyOpen
  ps <- Parser.someTill (Parser.try prop <|> selector) (char '}')
  whitespace
  pure $ AtRule rule (Text.strip name) ps


prop :: Parser Value
prop =
  Prop <$> propName <*> propVal


propName :: Parser Text
propName = do
  reserved <- optional (Parser.oneOf ['&', '>', '<'])
  case reserved of
    Just _ ->
      empty
    Nothing ->
      Parser.takeWhileP (Just "prop") (\t -> t /= ':' && t /= ' ')
      <* surround whitespace colon


propVal :: Parser Text
propVal = do
  v <- Parser.takeWhileP (Just "propVal") (\t -> t /= '#' && t /= '}' && t /= ';')
  maybeHashVar <- optional hashVar
  case maybeHashVar of
    Nothing -> do
      surround whitespace semicolon
      pure v
    Just hashVar' -> do
      v2 <- propVal
      pure (v <> hashVar' <> v2)


hashVar :: Parser Text
hashVar =
  (\a b c -> a <> b <> c)
  <$> string "#{"
  <*> Parser.takeWhileP (Just "hashVar") (/= '}')
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


whitespace :: Parser ()
whitespace =
  space <|> () <$ eol <|> () <$ newline


surround :: Parser a -> Parser b -> Parser b
surround a b =
  a *> b <* a
