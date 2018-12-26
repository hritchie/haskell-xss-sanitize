module Options where

import           Data.Monoid         ((<>))
import           Options.Applicative

data ProgramMode = Sanitize | Filter | PubSub deriving Read

options :: ParserInfo ProgramMode
options =
  let opts =
        ( option auto
            ( metavar "PubSub mode (default)"
            <> short 'p'
            <> long "pubsub"
            <> help "Subscribe to event channels and filter note and comment bodies"
            <> value PubSub)
        <|> option auto
              ( metavar "Filter mode"
              <> short 'f'
              <> long "filter"
              <> help "Print out problematic tags/attributes for given text"
              <> value Filter)
        <|> option auto
              ( metavar "Program Mode: Sanitize"
              <> short 's'
              <> long "sanitize"
              <> help "Filter and balance HTML text, and output result"
              <> value Sanitize)
        <|> pure PubSub)
  in
    info
    (opts <**> helper)
    ( fullDesc
      <> progDesc "sanitizes HTML input"
      <> header "XSS sanitizer")
