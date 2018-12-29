module Text.HTML.SanitizeXSS.Types where
import Control.Monad.Writer
import Control.Monad.RWS.Lazy
import           Data.Text                 (Text)
import           Text.HTML.TagSoup



-- Use to report errors
data XssFlag = XssFlag Text -- to develop
  deriving (Show, Eq)

-- TODO change to Seq or something more performant
-- TODO we want state monad so we know if we are IN an unsafe tag like script
-- because it is better to suppress and report the content of script tag

-- are we in an unsafe tag?
type XssState = Bool

type XssReader = ()

type XssRWS = RWS XssReader [XssFlag] XssState

type XssTagFilter = [Tag Text] -> XssRWS [Tag Text]



