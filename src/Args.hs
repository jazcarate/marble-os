module Args where

import           Prelude                 hiding ( repeat )
import           Options.Applicative            ( Parser
                                                , strArgument
                                                , long
                                                , metavar
                                                , help
                                                , value
                                                , showDefault
                                                , short
                                                , action
                                                , eitherReader
                                                , option
                                                , execParser
                                                , info
                                                , progDesc
                                                , fullDesc
                                                , helper
                                                , header
                                                , (<**>)
                                                , auto
                                                , subparser
                                                , hidden
                                                , command
                                                , showDefaultWith
                                                , str
                                                , readerAbort
                                                , completeWith
                                                , ParseError(ErrorMsg)
                                                )
import           Control.Applicative            ( (<|>)
                                                , optional
                                                )
import qualified Duration                      as D
import qualified Data.String                   as S
import qualified Configuration                 as C
import           Data.Default                   ( def )
import qualified Text.Read                     as R
import           Mbl                            ( parseRepeat )
import           Data.Char                      ( toLower )

remote :: Parser C.Remote
remote =
  C.Remote
    <$> option
          auto
          (  long "port"
          <> short 'p'
          <> help "Port to run the daemon."
          <> metavar "PORT"
          <> showDefault
          <> value 3000
          )
    <*> (C.Host <$> option
          str
          (  long "host"
          <> short 'h'
          <> help "Host to run the daemon."
          <> metavar "HOST"
          <> showDefault
          <> (value $ C.unHost def)
          )
        )

daemon :: Parser C.DaemonConfiguration
daemon = C.DaemonConfiguration <$> daemonSubCmd <*> remote

inspect :: Parser C.InspectConfiguration
inspect = C.InspectConfiguration <$> run

daemonSubCmd :: Parser C.DaemonSubConfiguration
daemonSubCmd =
  subparser
    $  command
         "list"
         (  C.List
         <$ (info (pure () <**> helper)
                  (fullDesc <> progDesc "List all mbls currently waiting.")
            )
         )
    <> command
         "start"
         (  C.Start
         <$ (info (pure () <**> helper)
                  (fullDesc <> progDesc "Start all mbls currently waiting.")
            )
         )

sync :: Parser C.SyncConfiguration
sync = C.SyncConfiguration <$> run <*> remote

source :: Parser C.Source
source =
  (C.File <$> strArgument
      (metavar "CONFIG" <> help "Target marble config file" <> action "file")
    )
    <|> (C.Inline <$> option
          str
          (  long "inline"
          <> short 'i'
          <> help "Alternatively, you can provide an inline mbl format"
          <> metavar "MBL"
          )
        )

parser :: Parser C.ParseConfiguration
parser =
  C.ParseConfiguration
    <$> option
          (eitherReader parseDelimiter)
          (  long "delimiter"
          <> short 'd'
          <> metavar "CHAR"
          <> help "The character to delimit ticks."
          <> showDefault
          <> value defaultDelimiter
          )
    <*> lane
    --Overrides
    <*> pure Nothing -- TODO Add name to sync?
    <*> optional
          (C.TickRate <$> option
            (eitherReader parseDuration)
            (long "tick" <> short 't' <> metavar "DURATION" <> help
              "Duration of each tick."
            )
          )
    <*> optional repeat
 where
  defaultDelimiter = '-'
  parseDuration :: String -> Either String D.Duration
  parseDuration = D.parseDuration . S.fromString
  parseDelimiter :: String -> Either String C.Delimiter
  parseDelimiter d = case d of
    x : [] -> Right x
    _      -> Left $ "Invalid delimiter `" <> d <> "`. Must be 1 character."

repeat :: Parser C.Repeat
repeat = option
  (   eitherReader parseRepeat --Keep this undocumented. It will be our own little secret.
  <|> eitherReader readRepeatOption
  <|> (C.Repeat <$> eitherReader parseNumber)
  <|> readerAbort (ErrorMsg extraHelp)
  )
  (  long "repeat"
  <> short 'r'
  <> metavar "TIMES"
  <> help
       ("Whether to repeat the sequence one it finishes." <> " " <> extraHelp)
  <> completeWith ["no", "loop", "1", "2"]
  )
  where extraHelp = "Possible values: `no`, `loop` or number of times."

readRepeatOption :: String -> Either String C.Repeat
readRepeatOption s = case toLower <$> s of
  "no"   -> pure C.Once
  "loop" -> pure C.Infinite
  _      -> Left "Could not parse repeat option"

lane :: Parser C.Lane
lane =
  (option
      ((C.Numbered <$> eitherReader parseNumber) <|> (C.Named <$> str))
      (  long "lane"
      <> short 'l'
      <> short 'n'
      <> help
           "If the file is multi-line, what line should it use. -line count starts at 1-. You can also use the lane name."
      <> metavar "LINE_NUMBER"
      <> showDefaultWith (\(C.Numbered x) -> show x)
      <> value (C.Numbered 1)
      )
    )
    <|> (C.Named <$> option
          str
          (  long "name" -- For those people that like to *name* their lanes with numbers 
          <> help
               "If the file is multi-line, what lane name should it use. (named lanes start with a name and a `:')."
          <> metavar "NAME"
          )
        )

parseNumber :: String -> Either String Int
parseNumber = R.readEither

run :: Parser C.RunConfiguration
run = C.RunConfiguration <$> source <*> parser


args :: IO C.Configuration
args = execParser opts
 where
  opts = info
    (    (subparser
           (  command
               "run"
               (   C.Run
               <$> (info (run <**> helper)
                         (fullDesc <> progDesc "Run the marble file")
                   )
               )
           <> command
                "sync"
                (   C.Sync
                <$> (info
                      (sync <**> helper)
                      (  fullDesc
                      <> progDesc
                           "Start a daemon and wait to run the marble.\nLook at `marble daemon --help` for more information"
                      )
                    )
                )
           <> command
                "daemon"
                (   C.Daemon
                <$> (info
                      (daemon <**> helper)
                      (  fullDesc
                      <> progDesc
                           "Control the daemon to launch `sync`'ed marble clients."
                      )
                    )
                )
           <> command
                "inspect"
                (   C.Inspect
                <$> (info (inspect <**> helper)
                          (fullDesc <> progDesc "Inspect a source.")
                    )
                )
           <> hidden
           )
         )
    <**> helper
    )
    (fullDesc <> header "marble-os - Run things at your own pace")
