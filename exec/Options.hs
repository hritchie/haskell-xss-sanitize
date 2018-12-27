module Options where


import           Options.Applicative
import           Protolude           hiding (option)

data ProgramMode = Sanitize Text | Filter Text | PubSub | QuickScan deriving Read

options :: ParserInfo ProgramMode
options =
  let opts =
        ( pure PubSub <* switch
            (  short 'p'
            <> long "pubsub"
            <> help "Subscribe to event channels and filter note and comment bodies (default mode)" )
        <|> Filter <$> option str
              ( metavar "HTML"
              <> short 'f'
              <> long "filter"
              <> help "Print out problematic tags/attributes for given HTML text")
        <|> Sanitize <$> option str
              ( metavar "HTML"
              <> short 's'
              <> long "sanitize"
              <> help "Filter and balance HTML text, and output result")
        <|> pure QuickScan <* switch
              (  short 'q'
              <> long "quick-scan"
              <> help "Read HTML text on stdin, report result via exit code (0 ==> clean)")
        <|> pure PubSub)
  in
    info
    (opts <**> helper)
    ( fullDesc
      <> progDesc "sanitizes HTML input"
      <> header "XSS sanitizer")
