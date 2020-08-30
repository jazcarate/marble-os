{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

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
import           Data.Serialize                 ( Serialize )
import           GHC.Generics                   ( Generic )

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

newtype Microseconds = Microseconds { unMicro :: Int } deriving (Eq, Num, Integral, Enum, Real, Ord, Generic)
instance Serialize Microseconds
instance Show Microseconds where
  show = humanReadableDuration . toInt

humanReadableDuration :: Int -> String
humanReadableDuration us
  | us < 1_000 = if us > 0 then show us  ++ "us" else ""
  | us < 1_000_000 = let ms = us `div` 1_000 in if ms > 0 then show ms  ++ "ms" ++ humanReadableDuration (us `mod` 1_000) else ""
  | us < 60_000_000 = let s = us `div` 1_000_000 in if s > 0 then show s ++ "s" ++ humanReadableDuration (us `mod` 1_000_000) else ""
  | otherwise = let m = us `div` 60_000_000 in if m > 0 then show m  ++ "m" ++ humanReadableDuration (us `mod` 60_000_000)  else ""

toInt :: Microseconds -> Int
toInt = unMicro

-- Control-Concurrent.threadDelay wants microseconds 
toMicroseconds :: Duration -> Microseconds
toMicroseconds dur = Microseconds $ case dur of
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

-- Test helper :D
seconds :: Int -> Duration
seconds = DurationSecond . flip DurSecond Nothing
