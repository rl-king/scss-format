{-# LANGUAGE OverloadedStrings #-}
module Lib (run, dev) where

import Control.Applicative hiding (many)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List (foldl', sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void
import System.Environment
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


dev :: IO ()
dev = do
  stylesheet <- Text.readFile "style.scss"
  case Parser.runParser parser "" stylesheet of
    Left e ->
      putStrLn $ Parser.errorBundlePretty e
    Right r -> do
      putStrLn "\n======\n"
      Text.putStrLn (render r)
      putStrLn "\n======\n"
      Print.pPrint r


run :: IO ()
run = do
  args <- getArgs
  case args of
    [] ->
      putStrLn "Filepath is missing, I'm expecting and argument like 'style.scss'"
    f:_ -> do
      stylesheet <- Text.readFile f
      case Parser.runParser parser "" stylesheet of
        Left e ->
          putStrLn $ Parser.errorBundlePretty e
        Right r ->
          Text.putStrLn (render r)


-- RENDER


render :: [Value] -> Text
render =
  Text.strip . fst . foldl' renderValue ("", 0)


renderValue :: (Text, Int) -> Value -> (Text, Int)
renderValue (acc, i) value =
  case value of
    Selector name values ->
      ( acc
        <> "\n"
        <> indent i <> name <> " {\n"
        <> fst (foldl' renderValue ("", i + 1) (sortOn propsSorter values))
        <> indent i <> "}\n"
      , i
      )
    AtRule rule name [] ->
      ( acc
        <> "\n"
        <> indent i <> "@" <> rule <> " " <> name <> ";\n"
      , i
      )
    AtRule rule name values ->
      ( acc
        <> "\n"
        <> indent i
        <> "@" <> rule <> " " <> name <> " {\n"
        <> fst (foldl' renderValue ("", i + 1) (sortOn propsSorter values))
        <> indent i <> "}\n"
      , i
      )
    Prop name v ->
      ( acc <> indent i <> name <> ": " <> v <> ";\n"
      , i
      )


indent :: Int -> Text
indent i =
  Text.replicate i "    "


-- PARSER


parser :: Parser [Value]
parser =
  Parser.manyTill (atRule <|> selector) Parser.eof


selector :: Parser Value
selector = do
  name <- Parser.takeWhileP (Just "selector") (/= '{')
  surround whitespace curlyOpen
  ps <- Parser.manyTill (Parser.try prop <|> atRule <|> selector) (char '}')
  whitespace
  pure $ Selector (Text.strip name) ps


atRule :: Parser Value
atRule = do
  _ <- char '@'
  rule <- Parser.takeWhileP (Just "at rule") (/= ' ')
  name <- Parser.takeWhileP (Just "at rule name") (\t -> t /= ';' && t /= '{')
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


propsSorter :: Value -> Int
propsSorter value =
  case value of
    Prop name _ ->
      fromMaybe 0 (Map.lookup name sortedProps)
    _ ->
      maxBound


sortedProps :: HashMap Text Int
sortedProps =
  let
    addIndex (acc, i) x =
      ((x, i) : acc, i + 1)
  in
  Map.fromList . fst $ foldl' addIndex ([], 0)
  [
    "align-content"
  , "align-items"
  , "align-self"
  , "all"
  , "animation"
  , "animation-delay"
  , "animation-direction"
  , "animation-duration"
  , "animation-fill-mode"
  , "animation-iteration-count"
  , "animation-name"
  , "animation-play-state"
  , "animation-timing-function"
  , "backface-visibility"
  , "background"
  , "background-attachment"
  , "background-blend-mode"
  , "background-clip"
  , "background-color"
  , "background-image"
  , "background-origin"
  , "background-position"
  , "background-repeat"
  , "background-size"
  , "border"
  , "border-bottom"
  , "border-bottom-color"
  , "border-bottom-left-radius"
  , "border-bottom-right-radius"
  , "border-bottom-style"
  , "border-bottom-width"
  , "border-collapse"
  , "border-color"
  , "border-image"
  , "border-image-outset"
  , "border-image-repeat"
  , "border-image-slice"
  , "border-image-source"
  , "border-image-width"
  , "border-left"
  , "border-left-color"
  , "border-left-style"
  , "border-left-width"
  , "border-radius"
  , "border-right"
  , "border-right-color"
  , "border-right-style"
  , "border-right-width"
  , "border-spacing"
  , "border-style"
  , "border-top"
  , "border-top-color"
  , "border-top-left-radius"
  , "border-top-right-radius"
  , "border-top-style"
  , "border-top-width"
  , "border-width"
  , "box-decoration-break"
  , "box-shadow"
  , "box-sizing"
  , "break-after"
  , "break-before"
  , "break-inside"
  , "caption-side"
  , "caret-color"
  , "@charset"
  , "clear"
  , "clip"
  , "color"
  , "column-count"
  , "column-fill"
  , "column-gap"
  , "column-rule"
  , "column-rule-color"
  , "column-rule-style"
  , "column-rule-width"
  , "column-span"
  , "column-width"
  , "columns"
  , "content"
  , "counter-increment"
  , "counter-reset"
  , "cursor"
  , "direction"
  , "display"
  , "empty-cells"
  , "filter"
  , "flex"
  , "flex-basis"
  , "flex-direction"
  , "flex-flow"
  , "flex-grow"
  , "flex-shrink"
  , "flex-wrap"
  , "float"
  , "font"
  , "@font-face"
  , "font-family"
  , "font-feature-settings"
  , "@font-feature-values"
  , "font-kerning"
  , "font-language-override"
  , "font-size"
  , "font-size-adjust"
  , "font-stretch"
  , "font-style"
  , "font-synthesis"
  , "font-variant"
  , "font-variant-alternates"
  , "font-variant-caps"
  , "font-variant-east-asian"
  , "font-variant-ligatures"
  , "font-variant-numeric"
  , "font-variant-position"
  , "font-weight"
  , "grid"
  , "grid-area"
  , "grid-auto-columns"
  , "grid-auto-flow"
  , "grid-auto-rows"
  , "grid-column"
  , "grid-column-end"
  , "grid-column-gap"
  , "grid-column-start"
  , "grid-gap"
  , "grid-row"
  , "grid-row-end"
  , "grid-row-gap"
  , "grid-row-start"
  , "grid-template"
  , "grid-template-areas"
  , "grid-template-columns"
  , "grid-template-rows"
  , "hanging-punctuation"
  , "height"
  , "hyphens"
  , "image-rendering"
  , "@import"
  , "isolation"
  , "justify-content"
  , "@keyframes"
  , "top"
  , "right"
  , "bottom"
  , "left"
  , "letter-spacing"
  , "line-break"
  , "line-height"
  , "list-style"
  , "list-style-image"
  , "list-style-position"
  , "list-style-type"
  , "margin"
  , "margin-bottom"
  , "margin-left"
  , "margin-right"
  , "margin-top"
  , "max-height"
  , "max-width"
  , "@media"
  , "min-height"
  , "min-width"
  , "mix-blend-mode"
  , "object-fit"
  , "object-position"
  , "opacity"
  , "order"
  , "orphans"
  , "outline"
  , "outline-color"
  , "outline-offset"
  , "outline-style"
  , "outline-width"
  , "overflow"
  , "overflow-wrap"
  , "overflow-x"
  , "overflow-y"
  , "padding"
  , "padding-bottom"
  , "padding-left"
  , "padding-right"
  , "padding-top"
  , "page-break-after"
  , "page-break-before"
  , "page-break-inside"
  , "perspective"
  , "perspective-origin"
  , "pointer-events"
  , "position"
  , "quotes"
  , "resize"
  , "scroll-behavior"
  , "tab-size"
  , "table-layout"
  , "text-align"
  , "text-align-last"
  , "text-combine-upright"
  , "text-decoration"
  , "text-decoration-color"
  , "text-decoration-line"
  , "text-decoration-style"
  , "text-indent"
  , "text-justify"
  , "text-orientation"
  , "text-overflow"
  , "text-shadow"
  , "text-transform"
  , "text-underline-position"
  , "transform"
  , "transform-origin"
  , "transform-style"
  , "transition"
  , "transition-delay"
  , "transition-duration"
  , "transition-property"
  , "transition-timing-function"
  , "unicode-bidi"
  , "user-select"
  , "vertical-align"
  , "visibility"
  , "white-space"
  , "widows"
  , "width"
  , "word-break"
  , "word-spacing"
  , "word-wrap"
  , "writing-mode"
  , "z-index"
  ]
