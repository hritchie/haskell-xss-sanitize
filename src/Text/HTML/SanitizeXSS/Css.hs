{-

https://stackoverflow.com/questions/3607894/cross-site-scripting-in-css-stylesheets

https://stackoverflow.com/questions/476276/using-javascript-in-css#answer-482088

-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.SanitizeXSS.Css (
  sanitizeCSS
-- #ifdef TEST
, allowedCssAttributeValue
-- #endif
  ) where

import           Control.Applicative         (pure, (<|>))
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char                   (isDigit)
import           Data.Monoid                 ((<>))
import           Data.Set                    (Set, fromList, member)
import           qualified Data.HashSet  as H
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Lazy              (toStrict)
import           Data.Text.Lazy.Builder      (toLazyText)
import           Prelude                     hiding (takeWhile)
import           Text.CSS.Parse              (parseAttrs)
import           Text.CSS.Render             (renderAttrs)
import           Text.HTML.SanitizeXSS.Types

-- import FileLocation (debug, debugM)

-- this is a direct translation from sanitizer.py, except
--   sanitizer.py filters out url(), but this is redundant

sanitizeCSS :: Text -> XssRWS Text
sanitizeCSS css = do
    as <- filterM isSanitaryAttr =<< filterUrl =<< parseAttributes css
    pure . toStrict . toLazyText .  renderAttrs $ as
  where
    filterUrl :: [(Text,Text)] -> XssRWS [(Text,Text)]
    filterUrl = mapM filterUrlAttribute
      where
        filterUrlAttribute :: (Text, Text) -> XssRWS (Text, Text)
        filterUrlAttribute (prop,value) =
            case parseOnly rejectUrl value of
              Left _      -> pure (prop,value)
              Right noUrl -> filterUrlAttribute (prop, noUrl)

        rejectUrl = do
          pre <- manyTill anyChar (string "url")
          skipMany space
          _<-char '('
          skipWhile (/= ')')
          _<-char ')'
          rest <- takeText
          return $ T.append (T.pack pre) rest


    parseAttributes :: Text -> XssRWS [(Text, Text)]
    parseAttributes css' = case parseAttrs css' of
      Left err   -> do
          reportUnsafe $ "css parse error: " <> (T.pack . show $ err)
          pure []
      Right as -> pure as

    isSanitaryAttr :: (Text, Text) -> XssRWS Bool
    isSanitaryAttr (_, "") = pure False
    isSanitaryAttr ("",_)  = pure False
    isSanitaryAttr (prop, value)
      | (T.toLower prop) `H.member` allowed_css_properties = pure True
      | (T.takeWhile (/= '-') prop) `member` allowed_css_unit_properties &&
          all allowedCssAttributeValue (T.words value) = pure True
      | prop `H.member` allowed_svg_properties = pure True
      | otherwise = do
            reportUnsafe $
                let cssProp = prop <> ": " <> value
                in "will strip unrecognized css property: [" <> cssProp <> "]"
            pure False

    allowed_css_unit_properties :: Set Text
    allowed_css_unit_properties = fromList ["background","border","margin","padding"]

allowedCssAttributeValue :: Text -> Bool
allowedCssAttributeValue val =
  val `H.member` allowed_css_keywords ||
    case parseOnly allowedCssAttributeParser val of
        Left _  -> False
        Right b -> b
  where
    allowedCssAttributeParser = do
      rgb <|> hex <|> rgb <|> cssUnit

    aToF = fromList "abcdef"

    hex = do
      _ <- char '#'
      hx <- takeText
      return $ T.all (\c -> isDigit c || (c `member` aToF)) hx

    -- should have used sepBy (symbol ",")
    rgb = do
      _<- string "rgb("
      skipMany1 digit >> skipOk (== '%')
      skip (== ',')
      skipMany digit >> skipOk (== '%')
      skip (== ',')
      skipMany digit >> skipOk (== '%')
      skip (== ')')
      return True

    cssUnit = do
      skip isDigit
      skipOk isDigit
      skipOk (== '.')
      skipOk isDigit >> skipOk isDigit
      skipSpace
      unit <- takeText
      return $ T.null unit || (T.toLower unit) `H.member` allowed_css_attribute_value_units

skipOk :: (Char -> Bool) -> Parser ()
skipOk p = skip p <|> pure ()

allowed_css_attribute_value_units :: H.HashSet Text
allowed_css_attribute_value_units = H.fromList
  [ "cm", "em", "ex", "in", "mm", "pc", "pt", "px", "%", ",", "\\"]

-- We need case insensitive membership
-- https://stackoverflow.com/questions/17967371/are-property-values-in-css-case-sensitive/26860699#26860699

allowed_css_properties :: H.HashSet Text
allowed_css_properties = H.fromList $ acceptable_css_properties <> mso_css_properties
  where
    acceptable_css_properties = ["azimuth", "background-color",
      "border-bottom-color", "border-collapse", "border-color",
      "border-left-color", "border-right-color", "border-top-color", "clear",
      "color", "cursor", "direction", "display", "elevation", "float", "font",
      "font-family", "font-size", "font-style", "font-variant", "font-weight",
      "height", "letter-spacing", "line-height", "opacity", "overflow", "pause",
      "pause-after", "pause-before", "pitch", "pitch-range", "richness",
      "speak", "speak-header", "speak-numeral", "speak-punctuation",
      "speech-rate", "stress", "text-align", "text-decoration", "text-indent",
      "unicode-bidi", "vertical-align", "voice-family", "volume",
      "white-space", "width"]
    mso_css_properties = ["mso-list", "mso-fareast-language"]

allowed_css_keywords :: H.HashSet Text
allowed_css_keywords = H.fromList acceptable_css_keywords
  where
    acceptable_css_keywords = ["auto", "aqua", "black", "block", "blue",
      "bold", "both", "bottom", "brown", "center", "collapse", "dashed",
      "dotted", "fuchsia", "gray", "green", "!important", "italic", "left",
      "lime", "maroon", "medium", "none", "navy", "normal", "nowrap", "olive",
      "pointer", "purple", "red", "right", "solid", "silver", "teal", "top",
      "transparent", "underline", "white", "yellow"]

-- used in css filtering
allowed_svg_properties :: H.HashSet Text
allowed_svg_properties = H.fromList acceptable_svg_properties
  where
    acceptable_svg_properties = [ "fill", "fill-opacity", "fill-rule",
        "stroke", "stroke-width", "stroke-linecap", "stroke-linejoin",
        "stroke-opacity"]
