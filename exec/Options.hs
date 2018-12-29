module Options where


import           Options.Applicative
import           Protolude           hiding (option)

data SubCommand = 
        Sanitize
      | Filter Bool -- quickscan mode
      | NoOp -- just parse and render
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
        <> command "sanitize" (info (helper <*> pure Sanitize) 
                (progDesc "Filter and balance HTML text (on stdin), and output result"))
        <> command "noop" (info (helper <*> pure NoOp) 
                (progDesc "Just parse and render. For testing TagSoup"))
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

