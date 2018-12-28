module Text.HTML.SanitizeXSS.Types where
import Control.Monad.Writer
import           Data.Text                 (Text)
import           Text.HTML.TagSoup


-- Use to report errors
data XssFlag = XssFlag Text -- to develop

-- TODO change to Seq or something more performant
type XssWriter = Writer [XssFlag]

type XssTagFilter = [Tag Text] -> XssWriter [Tag Text]



