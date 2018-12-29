module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text             as T (unpack)
import qualified Data.Text.IO          as T (getContents)
import           Options.Applicative   (execParser)
import           System.Exit           (exitSuccess, exitWith, ExitCode(..))
import           Text.HTML.SanitizeXSS

import           RMS.MQ.Connection

import           DBConnect
import           Options
import           PubSub

main :: IO ()
main = do
  programMode <- execParser options
  case programMode of
    Filter quickScan -> do
      input <- T.getContents
      case flagXss input of
        [] -> exitSuccess
        xs -> do
          if quickScan
          then exitWith (ExitFailure 1)
          else mapM_ (putStrLn . show) xs

    Sanitize -> do
      input <- T.getContents
      putStrLn $ T.unpack $ sanitizeBalance input
    PubSub -> do
      waitForConnection
      pool <- getConnPool
      mqActions <- initPubSub

      subscribeAndFilter pool mqActions
      forever (threadDelay maxBound)
