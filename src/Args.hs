module Args where

import           Prelude                 hiding ( repeat )
import           Options.Applicative            ( Parser
                                                , strArgument
                                                , long
                                                , metavar
                                                , help
                                                , switch
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
                                                , command
                                                )

import qualified Duration                      as D
import qualified Data.String                   as S
import qualified Configuration                 as C
import           Data.Default                   ( def )

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
    <*> option
          auto
          (  long "host"
          <> short 'h'
          <> help "Host to run the daemon."
          <> metavar "HOST"
          <> showDefault
          <> value def
          )

daemon :: Parser C.DaemonConfiguration
daemon = C.DaemonConfiguration <$> daemonSubCmd <*> remote

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

run :: Parser C.RunConfiguration
run =
  C.RunConfiguration
    <$> strArgument
          (metavar "CONFIG" <> help "Target marble config file" <> action "file"
          )
    <*> option
          auto
          (  long "lane"
          <> short 'l'
          <> help
               "If the file is multi-line, what line should it use. (line count starts at 1)."
          <> metavar "LINE_NUMBER"
          <> showDefault
          <> value 1
          )
    <*> switch
          (long "repeat" <> short 'r' <> help
            "Whether to repeat the sequence one it finishes."
          )
    <*> option
          (eitherReader parseDuration)
          (  long "tick"
          <> short 't'
          <> metavar "DURATION"
          <> help "Duration of each tick."
          <> showDefault
          <> value defaultDuration
          )
    <*> option
          (eitherReader parseDelimiter)
          (  long "delimiter"
          <> short 'd'
          <> metavar "CHAR"
          <> help "The character to delimit ticks."
          <> showDefault
          <> value defaultDelimiter
          )
 where
  defaultDelimiter = '-'
  defaultDuration  = D.seconds 1
  parseDuration :: String -> Either String D.Duration
  parseDuration = D.parseDuration . S.fromString
  parseDelimiter :: String -> Either String C.Delimiter
  parseDelimiter d = case d of
    x : [] -> Right x
    _      -> Left $ "Invalid delimiter `" <> d <> "`. Must be 1 character."


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
           )
         )
    <**> helper
    )
    (fullDesc <> header "marble-os - Run things at your own pace")
