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
import           Mbl.Types

parse :: ByteString -> Either String MBL
parse = parseOnly (mbl <* endOfInput)

delimiter :: Char
delimiter = '-'

wait :: Parser Action
wait = char delimiter *> pure Wait

print :: Parser Action
print = Print <$> takeWhile1 (\c -> c /= delimiter)

mbl :: Parser MBL
mbl = many $ wait <|> print
