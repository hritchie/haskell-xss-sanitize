module Options where

import           Data.Monoid         ((<>))
import           Options.Applicative

data ProgramMode = Cmdln | PubSub deriving Read

data Options =
  Options { _mode :: ProgramMode
          }

options :: ParserInfo Options
options =
  let opts = Options <$>
        ( option auto
            ( metavar "Program Mode - Default: command line"
            <> short 'p'
            <> long "pubsub"
            <> help "Subscribe to event channels and filter notes"
            <> value PubSub)
        <|> pure Cmdln)
  in
    info
    (opts <**> helper)
    ( fullDesc
      <> progDesc "sanitizes HTML input"
      <> header "XSS sanitizer")
