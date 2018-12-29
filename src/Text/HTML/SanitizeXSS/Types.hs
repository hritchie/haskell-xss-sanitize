{-# LANGUAGE TemplateHaskell #-}
module Text.HTML.SanitizeXSS.Types where
import Control.Monad.RWS.Lazy
import           Data.Text                 (Text)
import           Text.HTML.TagSoup
import Control.Lens



-- Use to report errors
data XssFlag = XssFlag Text -- to develop
  deriving (Show, Eq)

-- TODO change to Seq or something more performant
-- TODO we want state monad so we know if we are IN an unsafe tag like script
-- because it is better to suppress and report the content of script tag

-- are we in an unsafe tag?
data XssState = XssState {
    _unsanitaryTagStack :: [Tag Text] -- stack of open unsanitary tags
  , _lastOpenTagPosition :: Maybe (Int, Int) -- row, col
  } deriving Show

data XssConfig = XssConfig {
    _parseOptions :: ParseOptions Text
  } 

type XssRWS = RWS XssConfig [XssFlag] XssState

type XssTagFilter = [Tag Text] -> XssRWS [Tag Text]


makeLenses ''XssState
