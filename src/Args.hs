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
                                                )

import qualified Duration                      as D
import qualified Data.String                   as S
import           Configuration

configuration :: Parser Configuration
configuration =
  Configuration
    <$> strArgument
          (metavar "CONFIG" <> help "Target marble config file" <> action "file"
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
  parseDelimiter :: String -> Either String Delimiter
  parseDelimiter d = case d of
    x : [] -> Right x
    _      -> Left $ "Invalid delimiter" <> d


args :: IO Configuration
args = execParser opts
 where
  opts = info
    (configuration <**> helper)
    (fullDesc <> progDesc "Output things in a controlled manner" <> header
      "marble-os - Run things at your own pace"
    )
