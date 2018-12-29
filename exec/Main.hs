module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text             as T (unpack)
import qualified Data.Text.IO          as T (getContents, putStrLn)
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
      T.putStrLn $ sanitizeBalance input
    NoOp -> do
      input <- T.getContents
      T.putStrLn $ noOp input
    Parse -> do
      input <- T.getContents
      mapM_ (putStrLn . show) $ parse input

    PubSub -> do
      waitForConnection
      pool <- getConnPool
      mqActions <- initPubSub

      subscribeAndFilter pool mqActions
      forever (threadDelay maxBound)
