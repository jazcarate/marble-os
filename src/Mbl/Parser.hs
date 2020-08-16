module Mbl.Parser where

import           Prelude                 hiding ( takeWhile
                                                , print
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
import qualified Configuration as C
import           Mbl.Types

parse :: C.Configuration -> ByteString -> Either String MBL
parse conf = parseOnly (mbl conf <* endOfInput)


wait :: C.Delimiter -> Parser Action
wait delimiter = char delimiter *> pure Wait

print :: C.Delimiter -> Parser Action
print delimiter= Print <$> takeWhile1 (\c -> c /= delimiter)

mbl :: C.Configuration -> Parser MBL
mbl conf = many $ wait delimiter <|> print delimiter
    where delimiter = C.delimiter conf
