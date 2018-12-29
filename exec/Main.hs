module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text             as T (unpack)
import qualified Data.Text.IO          as T (getContents, putStrLn)
import           Options.Applicative   (execParser)
import           System.Exit           (ExitCode (..), exitSuccess, exitWith)
import           Text.HTML.SanitizeXSS

import           RMS.MQ.Connection

import           Data.Monoid
import           DBConnect
import           Options.Applicative
import           PubSub


data SubCommand =
        Sanitize
      | Filter Bool -- quickscan mode
      | NoOp -- just parse and render
      | Parse -- output parse stream
      | PubSub
      | QuickScan
      deriving Read

options :: ParserInfo SubCommand
options =
  let opts =
        subparser
        (
           command "pubsub" (info (helper <*> pure PubSub)
                (progDesc "Subscribe to event channels and filter note and comment bodies (default mode)"))
        <> command "filter" (info (helper <*> filterSubcommand)
                (progDesc "Print out problematic tags/attributes for HTML text on stdin"))
        <> command "flag" (info (helper <*> filterSubcommand)
                (progDesc "Print out problematic tags/attributes for HTML text on stdin"))
        <> command "sanitize" (info (helper <*> pure Sanitize)
                (progDesc "Filter and balance HTML text (on stdin), and output result"))
        <> command "noop" (info (helper <*> pure NoOp) (progDesc "Just parse and render. For testing TagSoup."))
        <> command "parse" (info (helper <*> pure Parse) (progDesc "Just parse. For testing TagSoup."))
        )
  in
    info
    (opts <**> helper)
    ( fullDesc
      <> progDesc "sanitizes HTML input"
      <> header "XSS sanitizer")


filterSubcommand =
        Filter
          <$> switch (short 'q'
                      <> long "quick-scan"
                      <> help "Read HTML text on stdin, report result via exit code (0 ==> clean)")


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
