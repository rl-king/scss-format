{-# LANGUAGE OverloadedStrings #-}
module Language.Scss.Format
  ( format
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Language.Scss.Parser as Parser


format :: [Value] -> Text
format values =
  Text.strip . toStrict . B.toLazyText . mconcat
  $ List.zipWith (renderValue 0) (Nothing : fmap Just values) values


renderValueList :: Int -> [Value] -> Builder
renderValueList depth values =
  mconcat $ List.zipWith
  (renderValue (depth + 1))
  (Nothing : fmap Just nestedValues) nestedValues
  where
    nestedValues =
      List.sortOn propsSorter values


renderValue :: Int -> Maybe Value -> Value -> Builder
renderValue depth previous current =
  case current of
    Selector name values ->
      addNewLine depth previous current
      <> indent depth <> B.fromText name <> " {"
      <> newline
      <> renderValueList depth values
      <> indent depth
      <> "}"
      <> newline
    AtRule rule name [] ->
      addNewLine depth previous current
      <> indent depth
      <> "@" <> B.fromText rule <> " " <> B.fromText name <> ";"
      <> newline
    AtRule rule name values ->
      addNewLine depth previous current
      <> indent depth
      <> "@" <> B.fromText rule <> " " <> B.fromText name <> " {"
      <> newline
      <> renderValueList depth values
      <> indent depth
      <> "}"
      <> newline
    Prop name v ->
      indent depth
      <> B.fromText name <> ": " <> B.fromText v <> ";"
      <> newline
    Variable name v ->
      addNewLine depth previous current
      <> indent depth
      <> "$" <> B.fromText name <> ": " <> B.fromText v <> ";"
      <> newline
    MultilineComment comment ->
      addNewLine depth previous current
      <> indent depth
      <> "/*" <> B.fromText comment <> "*/"
      <> newline
    Comment comment ->
      addNewLine depth previous current
      <> indent depth
      <> "//" <> B.fromText comment
      <> newline


newline :: Builder
newline =
  B.singleton '\n'


indent :: Int -> Builder
indent i =
  B.fromText $ Text.replicate i "    "


addNewLine :: Int -> Maybe Value -> Value -> Builder
addNewLine depth previous current =
  case (previous, current) of
    (Nothing, _) -> mempty
    (Just (AtRule _ _ []), AtRule _ _ []) -> mempty
    (Just (Variable _ _), Variable _ _) -> mempty
    (_, Comment _)
      | depth == 0 -> newline <> newline
      | depth /= 0 -> mempty
      | otherwise -> newline
    (_, MultilineComment _)
      | depth == 0 -> newline <> newline
      | otherwise -> newline
    _ -> newline


propsSorter :: Value -> Int
propsSorter value =
  case value of
    Prop name _ ->
      fromMaybe 0 (Map.lookup name sortedProps)
    Variable _ _ ->
      -2
    AtRule _ _ [] ->
      -1
    AtRule{} ->
      maxBound
    Selector _ _ ->
      maxBound - 1
    MultilineComment _ ->
      0
    Comment _ ->
      0


sortedProps :: HashMap Text Int
sortedProps =
  let
    addIndex (acc, i) x =
      ((x, i) : acc, i + 1)
  in
  Map.fromList . fst $ List.foldl' addIndex ([], 0)
  [
    "content"
  , "width"
  , "min-width"
  , "max-width"

  , "height"
  , "min-height"
  , "max-height"

  , "box-sizing"

  , "padding"
  , "padding-top"
  , "padding-right"
  , "padding-bottom"
  , "padding-left"

  , "margin"
  , "margin-top"
  , "margin-right"
  , "margin-bottom"
  , "margin-left"

  , "opacity"

  , "display"

  , "flex"
  , "flex-grow"
  , "flex-shrink"
  , "flex-basis"
  , "place-self"
  , "align-self"
  , "order"

  , "flex-direction"
  , "flex-wrap"
  , "flex-flow"

  , "grid"
  , "grid-template"
  , "grid-template-columns"
  , "grid-template-rows"
  , "grid-template-areas"
  , "grid-auto-columns"
  , "grid-auto-rows"
  , "grid-auto-flow"
  , "grid-column-start"
  , "grid-column-end"
  , "grid-column"
  , "grid-row-start"
  , "grid-row-end"
  , "grid-row"
  , "grid-column-gap"
  , "grid-row-gap"
  , "grid-gap"
  , "grid-area"
  , "place-items"
  , "align-items"
  , "align-content"
  , "justify-items"
  , "justify-content"

  , "position"
  , "top"
  , "right"
  , "bottom"
  , "left"
  , "clip"

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

  , "box-shadow"

  , "list-style"
  , "list-style-image"
  , "list-style-position"
  , "list-style-type"

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
  , "letter-spacing"
  , "line-break"
  , "line-height"
  , "quotes"
  , "text-align"
  , "text-align-last"
  , "text-combine-upright"
  , "text-decoration"
  , "text-decoration-color"
  , "text-decoration-line"
  , "text-decoration-style"
  , "text-underline-position"
  , "-webkit-font-smoothing"
  , "-moz-osx-font-smoothing"
  , "text-indent"
  , "text-justify"
  , "text-orientation"
  , "text-overflow"
  , "text-shadow"
  , "text-transform"
  , "direction"
  , "hanging-punctuation"
  , "word-break"
  , "word-spacing"
  , "word-wrap"
  , "white-space"
  , "overflow-wrap"
  , "hyphens"
  , "caret-color"
  , "color"

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

  , "object-fit"
  , "object-position"
  , "image-rendering"

  , "filter"
  , "mix-blend-mode"

  , "outline"
  , "outline-color"
  , "outline-offset"
  , "outline-style"
  , "outline-width"
  , "overflow"
  , "overflow-x"
  , "overflow-y"

  , "transform"
  , "transform-origin"
  , "transform-style"
  , "backface-visibility"
  , "perspective"
  , "perspective-origin"

  , "columns"
  , "column-count"
  , "column-fill"
  , "column-gap"
  , "column-rule"
  , "column-rule-color"
  , "column-rule-style"
  , "column-rule-width"
  , "column-span"
  , "column-width"
  , "break-after"
  , "break-before"
  , "break-inside"
  , "page-break-after"
  , "page-break-before"
  , "page-break-inside"
  , "box-decoration-break"
  , "orphans"
  , "caption-side"
  , "empty-cells"
  , "table-layout"
  , "counter-increment"
  , "counter-reset"
  , "cursor"
  , "pointer-events"
  , "z-index"
  , "isolation"
  , "resize"
  , "scroll-behavior"
  , "tab-size"
  , "unicode-bidi"
  , "user-select"
  , "vertical-align"
  , "visibility"
  , "widows"
  , "writing-mode"
  , "all"
  , "clear"
  , "float"

  , "transition"
  , "transition-delay"
  , "transition-duration"
  , "transition-property"
  , "transition-timing-function"

  , "animation"
  , "animation-delay"
  , "animation-direction"
  , "animation-duration"
  , "animation-fill-mode"
  , "animation-iteration-count"
  , "animation-name"
  , "animation-play-state"
  , "animation-timing-function"

  , "@charset"
  , "@import"
  , "@keyframes"
  , "@media"
  ]
