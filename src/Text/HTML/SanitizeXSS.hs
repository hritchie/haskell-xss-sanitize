{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Sanatize HTML to prevent XSS attacks.
--
-- See README.md <http://github.com/gregwebs/haskell-xss-sanitize> for more details.
module Text.HTML.SanitizeXSS
    (
    -- * Sanitize
      sanitize
    , sanitizeBalance
    , sanitizeXSS
    , getProblematicAttributes
    , flagXss -- newer version of above
    , noOp
    , parse

    -- * Custom filtering
    , filterTags
    , safeTags
    , balanceTags

    -- * Utilities
    , safeTagName
    , sanitizeAttribute
    , sanitaryURI
    ) where

import           Codec.Binary.UTF8.String    (encodeString)
import           Control.Lens
import           Control.Monad
import           Control.Monad.RWS.Lazy
import           Data.Char                   (toLower)
import           Data.Maybe                  (catMaybes)
import           Data.Set                    (Set, fromAscList, fromList,
                                              member, notMember, toList, (\\))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Network.URI                 (URI (..), escapeURIString,
                                              isAllowedInURI, parseURIReference,
                                              uriScheme)
import           Text.HTML.SanitizeXSS.Css
import           Text.HTML.SanitizeXSS.Types
import           Text.HTML.TagSoup



-- | print potentially problematic attributes
getProblematicAttributes :: Text -> [Tag Text]
getProblematicAttributes txt =
  let
    -- we must canonicalize case
    allTags = fromList $ canonicalizeTags . parseTags $ txt
    goodTags = fromList . parseTags $ sanitize txt
    filteredTags = allTags \\ goodTags
  in
    toList filteredTags

initState :: XssState
initState = XssState [] (0,0) (TagText "")

initConfig :: XssConfig
initConfig = XssConfig (parseOptions{ optTagPosition = True })

   
formatFlag :: XssFlag -> Text
formatFlag x = 
    let (row, col) = x ^. flagPosition 
        pos = T.pack $ "line " <> show row <> " col " <> show col
        tag = renderTags [x^.flagOpenTag]
        msg = x^.flagReason
    in pos <> " " <> msg <> ": " <> tag

flagXss :: Text -> [Text]
flagXss input =
    map formatFlag . snd 
      $ evalRWS (filterTags safeTags input) initConfig initState

noOp :: Text -> Text
noOp = renderTagsOptions renderOptions {
        optMinimize = \x -> x `member` voidElems -- <img><img> converts to <img />, <a/> converts to <a></a>
      } . canonicalizeTags . parseTags

parse :: Text -> [Tag Text]
parse = parseTags

-- | Sanitize HTML to prevent XSS attacks.  This is equivalent to @filterTags safeTags@.
sanitize :: Text -> Text
sanitize = sanitizeXSS

-- | alias of sanitize function
sanitizeXSS :: Text -> Text
sanitizeXSS input =
    let r = evalRWS (filterTags safeTags input) initConfig initState
    in fst r

-- | Sanitize HTML to prevent XSS attacks and also make sure the tags are balanced.
--   This is equivalent to @filterTags (balanceTags . safeTags)@.
sanitizeBalance :: Text -> Text
sanitizeBalance input = fst $ evalRWS (filterTags (balanceTags <=< safeTags) input) initConfig initState

-- | Filter which makes sure the tags are balanced.  Use with 'filterTags' and 'safeTags' to create a custom filter.
balanceTags :: XssTagFilter
balanceTags = balance []

-- | Parse the given text to a list of tags, apply the given filtering function, and render back to HTML.
--   You can insert your own custom filtering but make sure you compose your filtering function with 'safeTags'!
filterTags :: XssTagFilter -> Text -> XssRWS Text
filterTags f input = do
    parseOpts <- _parseOptions <$> ask
    tags <- f . canonicalizeTags . parseTagsOptions parseOpts $ input
    return $ renderTagsOptions renderOptions {
        -- <img><img> converts to <img />, <a/> converts to <a></a>
        optMinimize = \x -> x `member` voidElems
      } tags

voidElems :: Set T.Text
voidElems = fromAscList $ T.words $ T.pack "area base br col command embed hr img input keygen link meta param source track wbr"

balance :: [Text] -- ^ unclosed tags
        -> XssTagFilter
balance unclosed [] = pure $ map TagClose $ filter (`notMember` voidElems) unclosed
balance (x:xs) tags'@(TagClose name:tags)
    | x == name = (TagClose name :) <$>  balance xs tags
    | x `member` voidElems = balance xs tags'
    | otherwise = do
        ys <- balance (x:xs) tags
        pure $ TagOpen name [] : TagClose name : ys
balance unclosed (TagOpen name as : tags) =
    (TagOpen name as :) <$> balance (name : unclosed) tags
balance unclosed (t:ts) = (t :) <$> balance unclosed ts

-- | Filters out any usafe tags and attributes. Use with filterTags to create a custom filter.
safeTags :: XssTagFilter
safeTags [] = pure []
safeTags (t@(TagClose name):tags)
    | safeTagName name = (t:) <$> safeTags tags
    | otherwise = do
          unsanitaryTagStack %= drop 1
          safeTags tags
safeTags (x@(TagOpen name attributes):tags)
  | safeTagName name = do
        lastOpenTag .= x
        as <- mapM sanitizeAttribute attributes
        let t = TagOpen name (catMaybes as)
        (t :) <$> safeTags tags
  | otherwise = do
        unsanitaryTagStack .= [x]
        lastOpenTag .= x
        reportUnsafe "will strip unsafe tag"
        safeTags tags
safeTags ((TagPosition r c):tags) = do
    lastOpenTagPosition .= (r, c)
    -- drop other positions so that RenderOptions optMinimize = True works
    safeTags tags
safeTags (t@(TagText c):tags) = do
    inUnsafe <- _unsanitaryTagStack <$> get
    case inUnsafe of
      ((TagOpen "script" _):_) -> do
        -- TODO allow command line option determine whether to print entire script
        -- content or just the beginning
        reportUnsafe $ "will strip script tag content: " <> c
        safeTags tags
      _ -> (t:) <$> safeTags tags
safeTags (t:tags) = 
    (t:) <$> safeTags tags

safeTagName :: Text -> Bool
safeTagName = (`member` sanitaryTags)

-- | low-level API if you have your own HTML parser. Used by safeTags.
-- TODO change this to return also a list of unsanitary attrs with reasons

sanitizeAttribute :: (Text, Text) -> XssRWS (Maybe (Text, Text))
sanitizeAttribute ("style", value) = do
    css <- sanitizeCSS value
    pure $ if T.null css then Nothing else Just ("style", css)
sanitizeAttribute attr = do
    safe <- safeAttribute attr
    if safe
    then pure (Just attr)
    else pure Nothing

-- returns Nothing if attribute is safe, or a reason it's not
safeAttribute :: (Text, Text) -> XssRWS Bool
safeAttribute (name, value) = do
    -- TODO start writing flags here
    let isSanitaryAttr = name `member` sanitaryAttributes
        notUriAttr = name `notMember` uri_attributes
        isSanitaryUri = sanitaryURI value
        isOk = isSanitaryAttr && (notUriAttr || isSanitaryUri)
    when (not isOk) $ do
      if (not isSanitaryAttr)
      then
          reportUnsafe $ "will strip unsafe attr [" <> name <> "] from tag"
      else do
        when (not notUriAttr && not isSanitaryUri) $
          reportUnsafe $ "will strip unsafe uri attribute [" <> value <> "] from tag"
    pure isOk

-- | Returns @True@ if the specified URI is not a potential security risk.
sanitaryURI :: Text -> Bool
sanitaryURI u =
  case parseURIReference (escapeURI $ T.unpack u) of
     Just p  -> (schemeless p) ||
                (safeScheme p) ||
                (safeDataURI p)
     Nothing -> False
  where
    schemeless = null . uriScheme
    safeScheme x = (map toLower $ init $ uriScheme x) `member` safeURISchemes
    safeDataURI x = (isDataURI x) && (dataURIisSafe x)
    isDataURI x = ((map toLower $ init $ uriScheme x) == "data")
    dataURIisSafe x = member
                        (T.pack (takeWhile (\y -> y /= ';') (uriPath x)))
                        allowed_content_types


-- | Escape unicode characters in a URI.  Characters that are
-- already valid in a URI, including % and ?, are left alone.
escapeURI :: String -> String
escapeURI = escapeURIString isAllowedInURI . encodeString

safeURISchemes :: Set String
safeURISchemes = fromList acceptable_protocols

sanitaryTags :: Set Text
sanitaryTags = fromList (acceptable_elements ++ mathml_elements ++ svg_elements ++ mso_elements)
  \\ (fromList svg_allow_local_href) -- extra filtering not implemented

sanitaryAttributes :: Set Text
sanitaryAttributes = fromList (allowed_html_uri_attributes ++ acceptable_attributes ++ mathml_attributes ++ svg_attributes)
  \\ (fromList svg_attr_val_allows_ref) -- extra unescaping not implemented

allowed_html_uri_attributes :: [Text]
allowed_html_uri_attributes = ["href", "src", "cite", "action", "longdesc"]

allowed_content_types :: Set Text
allowed_content_types = fromList ["image/png", "image/jpeg", "image/gif", "image/webp", "image/bmp", "text/plain"]


uri_attributes :: Set Text
uri_attributes = fromList $ allowed_html_uri_attributes ++ ["xlink:href", "xml:base"]

acceptable_elements :: [Text]
acceptable_elements = ["a", "abbr", "acronym", "address", "area",
    "article", "aside", "audio", "b", "big", "blockquote", "br", "button",
    "canvas", "caption", "center", "cite", "code", "col", "colgroup",
    "command", "datagrid", "datalist", "dd", "del", "details", "dfn",
    "dialog", "dir", "div", "dl", "dt", "em", "event-source", "fieldset",
    "figcaption", "figure", "footer", "font", "form", "header", "h1", "h2",
    "h3", "h4", "h5", "h6", "hr", "i", "img", "input", "ins", "keygen",
    "kbd", "label", "legend", "li", "m", "main", "map", "menu", "meter", "multicol",
    "nav", "nextid", "ol", "output", "optgroup", "option", "p", "pre",
    "progress", "q", "s", "samp", "section", "select", "small", "sound",
    "source", "spacer", "span", "strike", "strong", "sub", "sup", "table",
    "tbody", "td", "textarea", "time", "tfoot", "th", "thead", "tr", "tt",
    "u", "ul", "var", "video"
    -- MACKEY
    , "style", "head", "body", "html"]

mso_elements :: [Text]
mso_elements = ["o:p"]


mathml_elements :: [Text]
mathml_elements = ["maction", "math", "merror", "mfrac", "mi",
    "mmultiscripts", "mn", "mo", "mover", "mpadded", "mphantom",
    "mprescripts", "mroot", "mrow", "mspace", "msqrt", "mstyle", "msub",
    "msubsup", "msup", "mtable", "mtd", "mtext", "mtr", "munder",
    "munderover", "none"]

-- this should include altGlyph I think
svg_elements :: [Text]
svg_elements = ["a", "animate", "animateColor", "animateMotion",
    "animateTransform", "clipPath", "circle", "defs", "desc", "ellipse",
    "font-face", "font-face-name", "font-face-src", "g", "glyph", "hkern",
    "linearGradient", "line", "marker", "metadata", "missing-glyph",
    "mpath", "path", "polygon", "polyline", "radialGradient", "rect",
    "set", "stop", "svg", "switch", "text", "title", "tspan", "use"]

acceptable_attributes :: [Text]
acceptable_attributes = ["abbr", "accept", "accept-charset", "accesskey",
    "align", "alt", "autocomplete", "autofocus", "axis",
    "background", "balance", "bgcolor", "bgproperties", "border",
    "bordercolor", "bordercolordark", "bordercolorlight", "bottompadding",
    "cellpadding", "cellspacing", "ch", "challenge", "char", "charoff",
    "choff", "charset", "checked", "class", "clear", "color",
    "cols", "colspan", "compact", "contenteditable", "controls", "coords",
    -- "data", TODO: allow this with further filtering
    "datafld", "datapagesize", "datasrc", "datetime", "default",
    "delay", "dir", "disabled", "draggable", "dynsrc", "enctype", "end",
    "face", "for", "form", "frame", "galleryimg", "gutter", "headers",
    "height", "hidefocus", "hidden", "high", "hreflang", "hspace",
    "icon", "id", 
    "itemprop", "itemscope", "itemtype",
    "inputmode", "ismap", "keytype", "label", "leftspacing",
    "lang", "list", "loop", "loopcount", "loopend",
    "loopstart", "low", "lowsrc", "max", "maxlength", "media", "method",
    "min", "multiple", "name", "nohref", "noshade", "nowrap", "open",
    "optimum", "pattern", "ping", "point-size", "prompt", "pqg",
    "radiogroup", "readonly", "rel", "repeat-max", "repeat-min",
    "replace", "required", "rev", "rightspacing", "rows", "rowspan",
    "rules", "scope", "selected", "shape", "size", "span", "start",
    "step",
    "style", -- gets further filtering
    "summary", "suppress", "tabindex", "target",
    "template", "title", "toppadding", "type", "unselectable", "usemap",
    "urn", "valign", "value", "variable", "volume", "vspace", "vrml",
    "width", "wrap", "xml:lang"]

  

acceptable_protocols :: [String]
acceptable_protocols = [ "ed2k", "ftp", "http", "https", "irc",
    "mailto", "news", "gopher", "nntp", "telnet", "webcal",
    "xmpp", "callto", "feed", "urn", "aim", "rsync", "tag",
    "ssh", "sftp", "rtsp", "afs" ]

mathml_attributes :: [Text]
mathml_attributes = ["actiontype", "align", "columnalign", "columnalign",
    "columnalign", "columnlines", "columnspacing", "columnspan", "depth",
    "display", "displaystyle", "equalcolumns", "equalrows", "fence",
    "fontstyle", "fontweight", "frame", "height", "linethickness", "lspace",
    "mathbackground", "mathcolor", "mathvariant", "mathvariant", "maxsize",
    "minsize", "other", "rowalign", "rowalign", "rowalign", "rowlines",
    "rowspacing", "rowspan", "rspace", "scriptlevel", "selection",
    "separator", "stretchy", "width", "width", "xlink:href", "xlink:show",
    "xlink:type", "xmlns", "xmlns:xlink"]

svg_attributes :: [Text]
svg_attributes = ["accent-height", "accumulate", "additive", "alphabetic",
    "arabic-form", "ascent", "attributeName", "attributeType",
    "baseProfile", "bbox", "begin", "by", "calcMode", "cap-height",
    "class", "clip-path", "color", "color-rendering", "content", "cx",
    "cy", "d", "dx", "dy", "descent", "display", "dur", "end", "fill",
    "fill-opacity", "fill-rule", "font-family", "font-size",
    "font-stretch", "font-style", "font-variant", "font-weight", "from",
    "fx", "fy", "g1", "g2", "glyph-name", "gradientUnits", "hanging",
    "height", "horiz-adv-x", "horiz-origin-x", "id", "ideographic", "k",
    "keyPoints", "keySplines", "keyTimes", "lang", "marker-end",
    "marker-mid", "marker-start", "markerHeight", "markerUnits",
    "markerWidth", "mathematical", "max", "min", "name", "offset",
    "opacity", "orient", "origin", "overline-position",
    "overline-thickness", "panose-1", "path", "pathLength", "points",
    "preserveAspectRatio", "r", "refX", "refY", "repeatCount",
    "repeatDur", "requiredExtensions", "requiredFeatures", "restart",
    "rotate", "rx", "ry", "slope", "stemh", "stemv", "stop-color",
    "stop-opacity", "strikethrough-position", "strikethrough-thickness",
    "stroke", "stroke-dasharray", "stroke-dashoffset", "stroke-linecap",
    "stroke-linejoin", "stroke-miterlimit", "stroke-opacity",
    "stroke-width", "systemLanguage", "target", "text-anchor", "to",
    "transform", "type", "u1", "u2", "underline-position",
    "underline-thickness", "unicode", "unicode-range", "units-per-em",
    "values", "version", "viewBox", "visibility", "width", "widths", "x",
    "x-height", "x1", "x2", "xlink:actuate", "xlink:arcrole",
    "xlink:href", "xlink:role", "xlink:show", "xlink:title", "xlink:type",
    "xml:base", "xml:lang", "xml:space", "xmlns", "xmlns:xlink", "y",
    "y1", "y2", "zoomAndPan"]

-- the values for these need to be escaped
svg_attr_val_allows_ref :: [Text]
svg_attr_val_allows_ref = ["clip-path", "color-profile", "cursor", "fill",
    "filter", "marker", "marker-start", "marker-mid", "marker-end",
    "mask", "stroke"]

svg_allow_local_href :: [Text]
svg_allow_local_href = ["altGlyph", "animate", "animateColor",
    "animateMotion", "animateTransform", "cursor", "feImage", "filter",
    "linearGradient", "pattern", "radialGradient", "textpath", "tref",
    "set", "use"]

