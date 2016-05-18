module Main where

import Text.HTML.SanitizeXSS
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (getContents)

main = do
  input <- T.getContents
  putStrLn $ T.unpack $ sanitize input
