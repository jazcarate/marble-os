{-# LANGUAGE NumericUnderscores #-}

module Duration where


import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser
                                                , decimal
                                                , string
                                                , char
                                                , endOfInput
                                                , parseOnly
                                                )
import           Data.ByteString                ( ByteString )

data DurMicrosecond = DurMicrosecond Int                     deriving (Eq, Ord, Show)
data DurMillisecond = DurMillisecond Int  (Maybe DurMicrosecond)                   deriving (Eq, Ord, Show)
data DurSecond = DurSecond Int  (Maybe DurMillisecond)                     deriving (Eq, Ord, Show)
data DurMinute = DurMinute Int (Maybe DurSecond)   deriving (Eq, Ord, Show)

data Duration  = DurationMicrosecond DurMicrosecond
               | DurationMillisecond DurMillisecond
               | DurationSecond DurSecond
               | DurationMinute DurMinute deriving (Eq, Show)

duration :: Parser Duration
duration =
  (DurationMicrosecond <$> durMicrosecond)
    <|> (DurationMillisecond <$> durMillisecond)
    <|> (DurationSecond <$> durSecond)
    <|> (DurationMinute <$> durMinute)
    <|> (DurationSecond <$> durDefault)


durMicrosecond :: Parser DurMicrosecond
durMicrosecond = DurMicrosecond <$> (decimal <* string "us")


durMillisecond :: Parser DurMillisecond
durMillisecond =
  DurMillisecond <$> (decimal <* string "ms") <*> optional durMicrosecond

durSecond :: Parser DurSecond
durSecond = DurSecond <$> (decimal <* char 's') <*> optional durMillisecond

durMinute :: Parser DurMinute
durMinute = DurMinute <$> (decimal <* char 'm') <*> optional durSecond

durDefault :: Parser DurSecond
durDefault = DurSecond <$> decimal <*> pure Nothing

parseDuration :: ByteString -> Either String Duration
parseDuration = parseOnly (duration <* endOfInput)


-- Control-Concurrent.threadDelay wants microseconds 
toMicroseconds :: Duration -> Int
toMicroseconds dur = case dur of
  DurationMicrosecond dus -> dusToUs dus
  DurationMillisecond dms -> dmsToUs dms
  DurationSecond      ds  -> dsToUs ds
  DurationMinute      dm  -> dmToUs dm
 where
  dusToUs :: DurMicrosecond -> Int
  dusToUs (DurMicrosecond us) = us

  dmsToUs :: DurMillisecond -> Int
  dmsToUs (DurMillisecond ms mbus) = ms * 1_000 + maybe 0 dusToUs mbus

  dsToUs :: DurSecond -> Int
  dsToUs (DurSecond s mbms) = s * 1_000_000 + maybe 0 dmsToUs mbms

  dmToUs :: DurMinute -> Int
  dmToUs (DurMinute m mbs) = m * 60_000_000 + maybe 0 dsToUs mbs

seconds :: Int -> Duration
seconds = DurationSecond . flip DurSecond Nothing
