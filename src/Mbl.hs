module Mbl where

import           Prelude                 hiding ( takeWhile
                                                , print
                                                , repeat
                                                , lines
                                                )
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser
                                                , endOfInput
                                                , parseOnly
                                                , char
                                                , notChar
                                                , many1
                                                , (<?>)
                                                )
import           Data.ByteString                ( ByteString )
import           Control.Applicative            ( many
                                                , (<|>)
                                                )
import           Configuration
import qualified Control.Concurrent            as C
import qualified Control.Monad                 as CM
import qualified Data.ByteString.Char8         as BS
import qualified Duration                      as D
import qualified Data.Bool                     as B
import qualified System.IO                     as S

data Action = Wait | Print ByteString deriving (Show, Eq)

type MBL = [Action]


interpret :: Configuration -> MBL -> IO ()
interpret config mlb = CM.forM_ (repeat' mlb) interpret'
 where
  repeat' :: MBL -> MBL
  repeat' = B.bool id cycle (repeat config)
  interpret' :: Action -> IO ()
  interpret' Wait        = C.threadDelay $ D.toMicroseconds $ tick config
  interpret' (Print str) = BS.putStrLn str >> S.hFlush S.stdout


parse :: Configuration -> ByteString -> Either String MBL
parse conf content = do
  let lines = BS.lines content
  let lane' = lane conf
  line <- if (length lines >= lane')
    then pure $ lines !! (lane' - 1)
    else Left "Not enough lines to parse"
  parseOnly (mbl conf <* endOfInput) line


wait :: Delimiter -> Parser Action
wait delim = char delim *> pure Wait <?> "Wait"

print :: Delimiter -> Parser Action
print delim =
  Print
    <$> BS.pack
    <$> many1 (char '\\' *> char delim <> print' <|> print')
    <?> "Print"
  where print' = notChar delim

mbl :: Configuration -> Parser MBL
mbl conf = (many $ wait delim <|> print delim) <?> "One Line"
  where delim = delimiter conf
