{-# LANGUAGE DeriveGeneric #-}

module Mbl where

import           Prelude                 hiding ( takeWhile
                                                , print
                                                , repeat
                                                , lines
                                                )
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser
                                                , char
                                                , notChar
                                                , many1
                                                , (<?>)
                                                , parseOnly
                                                , skipSpace
                                                , endOfInput
                                                , scan
                                                , endOfLine
                                                , option
                                                )
import           Data.ByteString                ( ByteString )
import           Control.Applicative            ( many
                                                , (<|>)
                                                )
import           Configuration                  ( RunConfiguration(..)
                                                , Lane(..)
                                                )
import qualified Control.Concurrent            as C
import qualified Control.Monad                 as CM
import qualified Data.Map.Strict               as Map
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Internal      as BSi
import qualified Duration                      as D
import qualified Data.Bool                     as B
import qualified System.IO                     as S
import           Data.Serialize                 ( Serialize )
import qualified Data.Bifunctor                as Bi
import           GHC.Generics
import           Data.Attoparsec.Text           ( isEndOfLine )
import qualified Lens.Micro                    as L
import           Data.List                      ( find )
import           Lens.Micro                     ( (&)
                                                , (^.)
                                                , (%~)
                                                )
import qualified Data.Text.Encoding            as T

data Action = Wait | Print ByteString deriving (Show, Eq, Generic)

newtype Name = Name { unName :: ByteString } deriving (Show, Eq, Generic)
data MBL = MBL { name :: Maybe Name, actions :: [Action]  } deriving (Show, Generic)

actionsL :: L.Lens' MBL [Action]
actionsL = L.lens actions (\mbl' as -> mbl' { actions = as })

instance Serialize Action
instance Serialize MBL
instance Serialize Name


interpret :: RunConfiguration -> MBL -> IO ()
interpret config mlb' = CM.forM_ (repeat' mlb' ^. actionsL) interpret'
 where
  repeat' :: MBL -> MBL
  repeat' mbl'' = mbl'' & actionsL %~ B.bool id cycle (repeat config)
  interpret' :: Action -> IO ()
  interpret' Wait        = C.threadDelay $ D.toMicroseconds $ tick config
  interpret' (Print str) = BS.putStrLn str >> S.hFlush S.stdout


preParser :: ByteString -> ByteString
preParser = BS.filter (/= '\r') . (<> "\n\n")

runParser :: RunConfiguration -> ByteString -> Either String MBL
runParser conf content = do
  mbls' <- parseAll conf content
  choose (lane conf) mbls'

choose :: Lane -> [MBL] -> Either String MBL
choose lane' mbls' = case lane' of
  Numbered laneNumber -> if length mbls' >= laneNumber
    then Right $ mbls' !! (laneNumber - 1)
    else Left "Not enough lines to parse"
  Named laneName ->
    maybe (Left $ "No lane with the name " <> show laneName) Right
      $ find (hasName $ T.encodeUtf8 laneName) mbls'
   where
    hasName :: ByteString -> MBL -> Bool
    hasName needle mbl' = maybe False (\n -> unName n == needle) $ name mbl'

parseAll :: RunConfiguration -> ByteString -> Either String [MBL]
parseAll conf content = do
  mblsWithRef <- parseOnly (lines conf) (preParser content)
  let mbls' = bindRefs $ toRefs mblsWithRef
  return mbls'

data MBLLine = MBLCandidate MBL | RefLine RefLine

type RefLine = (ByteString, ByteString)

data IntermediateMBL = IntermediateMBL { candidateMbls :: [MBL], refs :: Map.Map ByteString ByteString }

toRefs :: [MBLLine] -> IntermediateMBL
toRefs mblLines = IntermediateMBL mbls' refs'
 where
  mbls' = [ x | (MBLCandidate x) <- mblLines ]
  refs' = Map.fromList [ x | (RefLine x) <- mblLines ]

bindRefs :: IntermediateMBL -> [MBL]
bindRefs refs' = candidateMbls refs' & L.each . actionsL . L.each %~ bindAction
 where
  bindAction :: Action -> Action
  bindAction action = case action of
    Print candidate -> maybe action Print (Map.lookup candidate (refs refs'))
    _               -> action


ref :: RunConfiguration -> Parser (ByteString, ByteString)
ref _ =
  Bi.bimap BS.pack unescape
    <$> (   (,)
        <$> (  char '['
            *> many1 (notChar ']')
            <* char ']'
            <* skipSpace
            <* char ':'
            <* skipSpace
            )
        <*> takeWhile1_ (not . end)
        <?> "One Ref line"
        )
  where end c = isEndOfLine c

print :: RunConfiguration -> Parser Action
print conf = Print <$> unescape <$> takeWhile1_ (not . end) <?> "Print"
 where
  end c = c == delim || isEndOfLine c
  delim = delimiter conf

wait :: RunConfiguration -> Parser Action
wait conf = char delim *> pure Wait <?> "Wait" where delim = delimiter conf

lines :: RunConfiguration -> Parser [MBLLine]
lines conf = many (line conf <* many1 endOfLine <* option () endOfInput)

line :: RunConfiguration -> Parser MBLLine
line conf =
  (RefLine <$> ref conf <|> (MBLCandidate <$> mbl conf)) <?> "Ref Lines"

mbl :: RunConfiguration -> Parser MBL
mbl conf =
  MBL
    <$> maybeOption name'
    <*> many1 (wait conf <|> print conf)
    <?> "One MBL line"

name' :: Parser Name
name' = Name <$> strip <$> takeWhile1_ (not . end) <* char ':' <* skipSpace
 where
  end c = c == ':' || isEndOfLine c
  strip :: ByteString -> ByteString
  strip s = fst $ BS.spanEnd (BSi.isSpaceChar8) s

-- | Make a parser optional, return Nothing if there is no match
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

-- | Like `takeWhile`, but unconditionally take escaped characters.
takeWhile_ :: (Char -> Bool) -> Parser BS.ByteString
takeWhile_ p = scan False p_
 where
  p_ escaped c | escaped   = Just False
               | not $ p c = Nothing
               | otherwise = Just (c == '\\')

-- | Like `takeWhile1`, but unconditionally take escaped characters.
takeWhile1_ :: (Char -> Bool) -> Parser BS.ByteString
takeWhile1_ = CM.mfilter (not . BS.null) . takeWhile_

unescape :: ByteString -> ByteString
unescape = BS.pack . unescape' . BS.unpack
 where
  unescape' ""              = ""
  unescape' ('\\' : x : xs) = x : unescape' xs
  unescape' (x        : xs) = x : unescape' xs
