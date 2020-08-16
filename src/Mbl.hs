module Mbl where

import           Prelude                 hiding ( takeWhile
                                                , print
                                                , repeat
                                                )
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser
                                                , endOfInput
                                                , parseOnly
                                                , char
                                                , takeWhile1
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

data Action = Wait | Print ByteString deriving (Show, Eq)

type MBL = [Action]


interpret :: Configuration -> MBL -> IO ()
interpret config mlb = CM.forM_ (repeat' mlb) interpret'
 where
  repeat' :: MBL -> MBL
  repeat' = B.bool id cycle (repeat config)
  interpret' :: Action -> IO ()
  interpret' Wait        = C.threadDelay $ D.toMicroseconds $ tick config
  interpret' (Print str) = BS.putStrLn str


parse :: Configuration -> ByteString -> Either String MBL
parse conf = parseOnly (mbl conf <* endOfInput)


wait :: Delimiter -> Parser Action
wait delim = char delim *> pure Wait

print :: Delimiter -> Parser Action
print delim = Print <$> takeWhile1 (\c -> c /= delim)

mbl :: Configuration -> Parser MBL
mbl conf = many $ wait del <|> print del where del = delimiter conf
