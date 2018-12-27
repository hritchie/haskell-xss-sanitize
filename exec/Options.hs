module Options where


import           Options.Applicative
import           Protolude           hiding (option)

data ProgramMode = Sanitize Text | Filter Text | PubSub deriving Read

options :: ParserInfo ProgramMode
options =
  let opts =
        ( pure PubSub <* switch
            (  short 'p'
            <> long "pubsub"
            <> help "(default mode) Subscribe to event channels and filter note and comment bodies" )
        <|> Filter <$> option str
              ( metavar "(Filter mode)"
              <> short 'f'
              <> long "filter"
              <> help "Print out problematic tags/attributes for given text")
        <|> Sanitize <$> option str
              ( metavar "(Sanitize mode)"
              <> short 's'
              <> long "sanitize"
              <> help "Filter and balance HTML text, and output result")
        <|> pure PubSub)
  in
    info
    (opts <**> helper)
    ( fullDesc
      <> progDesc "sanitizes HTML input"
      <> header "XSS sanitizer")
