module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text             as T (unpack)
import qualified Data.Text.IO          as T (getContents)
import           Options.Applicative   (execParser)
import           Text.HTML.SanitizeXSS

import           RMS.MQ.Connection

import           DBConnect
import           Options
import           PubSub

main :: IO ()
main = do
  programMode <- execParser options
  case programMode of
    Filter input -> do
      mapM_ (putStrLn . show) (getProblematicAttributes input)
    Sanitize input -> do
      putStrLn $ T.unpack $ sanitize input
    PubSub -> do
      waitForConnection
      pool <- getConnPool
      mqActions <- initPubSub

      subscribeAndFilter pool mqActions
      forever (threadDelay maxBound)
