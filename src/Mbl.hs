{-# LANGUAGE DeriveGeneric #-}

module Mbl where

import           Prelude                 hiding ( takeWhile
                                                , print
                                                , repeat
                                                , lines
                                                )
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser
                                                , IResult(..)
                                                , parse
                                                , char
                                                , notChar
                                                , many1
                                                , (<?>)
                                                , skipSpace
                                                , anyChar
                                                , manyTill
                                                , satisfy
                                                , endOfInput
                                                , scan
                                                , feed
                                                , endOfLine
                                                , takeTill
                                                , option
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
import           Data.List                      ( intercalate )
import           GHC.Generics
import           Data.Attoparsec.Text           ( isEndOfLine )

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


runParser :: RunConfiguration -> ByteString -> Either String MBL
runParser conf content = do
  let lane' = lane conf
  mblsWithRef <- parseOnly (lines conf) (content <> "\n")
  let mbls' = bindRefs $ toRefs mblsWithRef
  if (length mbls' >= lane')
    then pure $ mbls' !! (lane' - 1)
    else Left "Not enough lines to parse"

parseOnly :: Parser a -> ByteString -> Either String a
parseOnly p s = case parse p s of
  Fail _ []   err -> Left err
  Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
  Done _ a        -> Right a
  Partial _       -> Left "More input needed"

data MBLLine = MBLCandidate MBL | RefLine RefLine deriving (Show)

type RefLine = (ByteString, ByteString)

data Ref = Ref { candidateMbls :: [MBL], refs :: Map.Map ByteString ByteString }

toRefs :: [MBLLine] -> Ref
toRefs mblLines = Ref mbls' refs'
 where
  mbls' = [ x | (MBLCandidate x) <- mblLines ]
  refs' = Map.fromList [ x | (RefLine x) <- mblLines ]

bindRefs :: Ref -> [MBL]
bindRefs refs' = bindAction <$$> candidateMbls refs'
 where
  (<$$>) = (<$>) . (<$>)
  bindAction :: Action -> Action
  bindAction action = case action of
    Print candidate -> maybe action Print (Map.lookup candidate (refs refs'))
    _               -> action


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
            <* option () (endOfLine)
            )
        <*> manyTill anyChar ((endOfLine <* char '[') <|> endOfInput)
        <?> "One Ref line"
        )


print :: Delimiter -> Parser Action
print delim = Print <$> takeTill end <?> "Print"
  where end c = c == delim || isEndOfLine c

wait :: Delimiter -> Parser Action
wait delim = char delim *> pure Wait <?> "Wait"

lines :: RunConfiguration -> Parser [MBLLine]
lines conf =
  many (RefLine <$> ref conf <|> (MBLCandidate <$> mbl conf)) <?> "Ref Lines"

mbl :: RunConfiguration -> Parser MBL
mbl conf =
  manyTill (wait delim <|> print delim) (endOfInput <|> endOfLine)
    <?> "One MBL line"
  where delim = delimiter conf


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
