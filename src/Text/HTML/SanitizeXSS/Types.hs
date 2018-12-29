{-# LANGUAGE TemplateHaskell #-}
module Text.HTML.SanitizeXSS.Types where
import Control.Monad.RWS.Lazy
import           Data.Text                 (Text)
import           Text.HTML.TagSoup
import Control.Lens



-- Use to report errors
data XssFlag = XssFlag {
      _flagPosition :: (Int, Int)
    , _flagOpenTag :: Tag Text
    , _flagReason :: Text -- position, open tag
    }
  deriving (Show, Eq)

makeLenses ''XssFlag


-- TODO change to Seq or something more performant
-- TODO we want state monad so we know if we are IN an unsafe tag like script
-- because it is better to suppress and report the content of script tag

-- are we in an unsafe tag?
data XssState = XssState {
    _unsanitaryTagStack :: [Tag Text] -- stack of open unsanitary tags
  , _lastOpenTagPosition :: (Int, Int) -- row, col
  , _lastOpenTag :: Tag Text
  } deriving Show

makeLenses ''XssState

data XssConfig = XssConfig {
    _parseOptions :: ParseOptions Text
  } 

type XssRWS = RWS XssConfig [XssFlag] XssState

type XssTagFilter = [Tag Text] -> XssRWS [Tag Text]


-- | decorates report with last open tag info
reportUnsafe :: Text -> XssRWS ()
reportUnsafe msg = do
    st <- get
    let pos = st ^. lastOpenTagPosition
    let tag = st ^. lastOpenTag
    tell [XssFlag pos tag msg]
 
