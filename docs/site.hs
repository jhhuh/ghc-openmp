{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlphaNum, isSpace)
import qualified Data.Text as T
import Hakyll
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (walk)

main :: IO ()
main = hakyll $ do
  match "style.css" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $
    compile templateBodyCompiler

  match "index.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        fixHeadingIds
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

-- Generate heading IDs matching Jekyll/kramdown convention.
-- Jekyll keeps leading numbers ("1-abstract"); pandoc strips them ("abstract").
fixHeadingIds :: Pandoc -> Pandoc
fixHeadingIds = walk go
  where
    go :: Block -> Block
    go (Header lvl (_, cls, kvs) ils) =
      Header lvl (jekyllId (extractText ils), cls, kvs) ils
    go b = b

    extractText :: [Inline] -> T.Text
    extractText = foldMap inlineText

    inlineText :: Inline -> T.Text
    inlineText (Str t) = t
    inlineText Space = " "
    inlineText SoftBreak = " "
    inlineText (Code _ t) = t
    inlineText (Emph is) = extractText is
    inlineText (Strong is) = extractText is
    inlineText (Strikeout is) = extractText is
    inlineText (Link _ is _) = extractText is
    inlineText _ = ""

    jekyllId :: T.Text -> T.Text
    jekyllId =
      T.intercalate "-"
        . T.words
        . T.filter (\c -> isAlphaNum c || isSpace c || c == '-')
        . T.toLower
