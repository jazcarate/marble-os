{-# LANGUAGE DeriveGeneric #-}

module Mbl where

import           Prelude                 hiding ( takeWhile
                                                , print
                                                , repeat
                                                , lines
                                                )
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser
                                                , parseOnly
                                                , char
                                                , notChar
                                                , many1
                                                , (<?>)
                                                , skipSpace
                                                , anyChar
                                                , manyTill
                                                , satisfy
                                                , endOfInput
                                                , endOfLine
                                                , isEndOfLine
                                                )
import           Data.ByteString                ( ByteString )
import           Control.Applicative            ( many
                                                , (<|>)
                                                )
import           Configuration                  ( RunConfiguration(..)
                                                , Delimiter
                                                )
import qualified Control.Concurrent            as C
import qualified Control.Monad                 as CM
import qualified Data.Map.Strict               as Map
import qualified Data.ByteString.Char8         as BS
import qualified Duration                      as D
import qualified Data.Bool                     as B
import qualified System.IO                     as S
import           Data.Serialize                 ( Serialize )
import qualified Data.Bifunctor                as Bi
import           GHC.Generics

data Action = Wait | Print ByteString deriving (Show, Eq, Generic)

type MBL = [Action]

instance Serialize Action


interpret :: RunConfiguration -> MBL -> IO ()
interpret config mlb = CM.forM_ (repeat' mlb) interpret'
 where
  repeat' :: MBL -> MBL
  repeat' = B.bool id cycle (repeat config)
  interpret' :: Action -> IO ()
  interpret' Wait        = C.threadDelay $ D.toMicroseconds $ tick config
  interpret' (Print str) = BS.putStrLn str >> S.hFlush S.stdout


parse :: RunConfiguration -> ByteString -> Either String MBL
parse conf content = do
  let lane' = lane conf
  mblsWithRef <- parseOnly (lines conf) content
  let mbls' = bindRefs $ toRefs mblsWithRef
  if (length mbls' >= lane')
    then pure $ mbls' !! (lane' - 1)
    else Left "Not enough lines to parse"


wait :: Delimiter -> Parser Action
wait delim = char delim *> pure Wait <?> "Wait"

print :: Delimiter -> Parser Action
print delim =
  Print
    <$> BS.pack
    <$> many (char '\\' *> char delim <|> satisfy end)
    <?> "Print"
 where
  end :: Char -> Bool
  end c = c /= delim && c /= '\n' && c /= '\r'

lines :: RunConfiguration -> Parser [MBLLine]
lines conf =
  many (RefLine <$> ref conf <|> (MBLCandidate <$> mbl conf)) <?> "Ref Lines"

data MBLLine = MBLCandidate MBL | RefLine RefLine

toRefs :: [MBLLine] -> Ref
toRefs mblLines = Ref mbls' refs'
 where
  mbls' = [ x | (MBLCandidate x) <- mblLines ]
  refs' = Map.fromList [ x | (RefLine x) <- mblLines ]

type RefLine = (ByteString, ByteString)

ref :: RunConfiguration -> Parser (ByteString, ByteString)
ref _ =
  Bi.bimap BS.pack BS.pack
    <$> (   (,)
        <$> (  char '['
            *> many1 (notChar ']')
            <* char ']'
            <* skipSpace
            <* char ':'
            <* skipSpace
            )
        <*> manyTill anyChar ((endOfLine <* char '[') <|> endOfInput)
        <?> "One Ref line"
        )


data Ref = Ref { candidateMbls :: [MBL], refs :: Map.Map ByteString ByteString }

bindRefs :: Ref -> [MBL]
bindRefs refs' = bindAction <$$> candidateMbls refs'
 where
  (<$$>) = (<$>) . (<$>)
  bindAction :: Action -> Action
  bindAction action = case action of
    Print candidate -> maybe action Print (Map.lookup candidate (refs refs'))
    _               -> action


mbl :: RunConfiguration -> Parser MBL
mbl conf =
  manyTill (wait delim <|> print delim) (endOfInput <|> endOfLine)
    <?> "One MBL line"
  where delim = delimiter conf
