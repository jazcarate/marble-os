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
                                                , string
                                                , scientific
                                                , skipWhile
                                                )
import           Data.ByteString                ( ByteString )
import           Control.Applicative            ( many
                                                , (<|>)
                                                )
import           Configuration                  ( ParseConfiguration(..)
                                                , Lane(..)
                                                , Repeat(..)
                                                , TickRate(..)
                                                )
import qualified Control.Concurrent            as C
import qualified Data.Char                     as Char
import qualified Control.Monad                 as CM
import qualified Data.Map.Strict               as Map
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Internal      as BSi
import qualified Duration                      as D
import qualified System.IO                     as S
import           Data.Serialize                 ( Serialize )
import qualified Data.Bifunctor                as Bi
import           GHC.Generics                   ( Generic )
import           Data.Attoparsec.Text           ( isEndOfLine )
import qualified Lens.Micro                    as L
import           Data.List                      ( find
                                                , intercalate
                                                , sortBy
                                                , intersperse
                                                )
import           Lens.Micro                     ( (&)
                                                , (^.)
                                                , (%~)
                                                )
import           Data.Default                   ( def )
import qualified Control.Monad.Trans.State.Lazy
                                               as T

data Action = Wait {  implicit :: Bool, time :: D.Microseconds } | Print ByteString deriving (Eq, Show, Generic)
type Name = ByteString
data MBL = MBL { name :: Maybe Name, actions :: [Action], repeat :: Repeat  } deriving (Generic, Show)

showMBL :: MBL -> String
showMBL m = showMBLs [m]

showMBLs :: [MBL] -> String
showMBLs mbls = intercalate
  "\n"
  (["tick: " ++ show minimumTick] ++ showMbls ++ [""] ++ showRefs refsMap)
 where
  minimumTick = case [ x | (Wait _ x) <- concatMap actions mbls ] of
    [] -> D.toMicroseconds $ unTickRate def
    ms -> foldl1 gcd ms
  maxNameLength = maximum $ (maybe 0 ((+ 2) . BS.length)) <$> name <$> mbls

  (showMbls, refsMap) = T.runState (CM.mapM storeRefs mbls) Map.empty
  storeRefs :: MBL -> T.State (Map.Map ByteString Char) String
  storeRefs m = do
    let as = scaleWait minimumTick (actions m)
    charActions <- CM.mapM storeLongActions as
    return $ intercalate
      ""
      [ maybe
          (replicate (maxNameLength) ' ')
          (\n ->
            BS.unpack n
              <> replicate (maxNameLength - BS.length n - 2) ' '
              <> ": "
          )
        $ name m
      , BS.unpack $ BS.intercalate "" $ charActions
      , show $ repeat m
      ]
  storeLongActions :: Action -> T.State (Map.Map ByteString Char) ByteString
  storeLongActions a' = case a' of
    Wait _ _ -> pure $ BS.pack $ showAction a'
    Print a  -> do
      map' <- T.get
      let maxChar = maximum $ '`' : (snd <$> Map.toList map')
      case Map.lookup a map' of
        Nothing -> do
          let newKey = nextChar maxChar
          T.modify (Map.insert a newKey)
          pure $ BS.singleton newKey
        Just key -> pure $ BS.singleton key

isPrint :: Action -> Bool
isPrint a = case a of
  Print _ -> True
  _       -> False

escape :: ByteString -> ByteString
escape = BS.concatMap escape'
 where
  escape' c = if c == '-' || c == '|' then BS.snoc "\\" c else BS.singleton c

scaleWait :: D.Microseconds -> [Action] -> [Action]
scaleWait g as = concatMap scale as
 where
  scale :: Action -> [Action]
  scale a = case a of
    Wait imp ms -> replicate (D.toInt $ ms `div` g) (Wait imp g)
    _           -> [a]

nextChar :: Char -> Char
nextChar c = Char.chr (Char.ord c + 1)

showRefs :: Map.Map ByteString Char -> [String]
showRefs m =
  showOneRef <$> (sortBy (\a b -> compare (snd a) (snd b)) (Map.toList m))

showOneRef :: (ByteString, Char) -> String
showOneRef (val, key) = "[" <> [key] <> "]: " <> BS.unpack val

showAction :: Action -> [Char]
showAction a = case a of
  Wait impl _ -> if impl then "" else "-"
  Print s     -> BS.unpack s

actionsL :: L.Lens' (MBL) [Action]
actionsL = L.lens actions (\mbl' as -> mbl' { actions = as })

instance Serialize Action
instance Serialize MBL


interpret :: MBL -> IO ()
interpret mbl' = do
  CM.forM_ (mbl'' ^. actionsL) interpret'
 where
  mbl'' :: MBL
  mbl'' = mbl' & actionsL %~ (repeatActions $ repeat mbl')
  interpret' :: Action -> IO ()
  interpret' (Wait _ micros) = C.threadDelay $ D.toInt micros
  interpret' (Print str    ) = BS.putStrLn str >> S.hFlush S.stdout


preParser :: ByteString -> ByteString
preParser = BS.filter (/= '\r') . (<> "\n\n")

runParser :: ParseConfiguration -> ByteString -> Either String MBL
runParser conf content = do
  mbls <- parseAll conf content
  mlb' <- choose (lane conf) mbls
  return $ mlb'

changeTick :: TickRate -> Action -> Action
changeTick tr a = case a of
  wait'@(Wait _ _) -> wait' { time = (D.toMicroseconds $ unTickRate tr) }
  _                -> a

repeatActions :: Repeat -> [a] -> [a]
repeatActions s = case s of
  Infinite -> cycle
  Repeat i -> concat . (replicate i)
  Once     -> id

choose :: Lane -> [MBL] -> Either String MBL
choose lane' mbls' = case lane' of
  Numbered laneNumber -> if length mbls' >= laneNumber
    then Right $ mbls' !! (laneNumber - 1)
    else Left "Not enough lines to parse"
  Named laneName ->
    maybe (Left $ "No lane with the name " <> show laneName) Right
      $ find (hasName laneName) mbls'
   where
    hasName :: ByteString -> MBL -> Bool
    hasName needle mbl' = maybe False ((==) needle) $ name mbl'

parseAll :: ParseConfiguration -> ByteString -> Either String [MBL]
parseAll conf content = do
  core         <- parseOnly (lines conf) (preParser content)
  intermediate <- toIntermediate core
  let mbls' = bindRefs intermediate
  return $ overrides <$> mbls'
 where
  overrides :: MBL -> MBL
  overrides = nameOverride' . tickRateOverride' . repeatStrategyOverride'
  nameOverride' :: MBL -> MBL
  nameOverride' = maybe id (\n m -> m { name = Just n }) (nameOverride conf)
  tickRateOverride' :: MBL -> MBL
  tickRateOverride' = maybe id
                            (\t m -> m & actionsL . L.each %~ (changeTick t))
                            (tickRateOverride conf)
  repeatStrategyOverride' :: MBL -> MBL
  repeatStrategyOverride' =
    maybe id (\r m -> m { repeat = r }) (repeatStrategyOverride conf)


type RefLine = (ByteString, ByteString)

data CoreAction = CoreWait { cImplicit :: Bool }| CorePrint ByteString -- Unrefed
data CoreMBL = CoreMBL { cName :: Maybe Name, cRepeat :: Repeat, cActions :: [CoreAction] }
data Core = MBLLine CoreMBL | RefLine RefLine | TickRateLine TickRate

lines :: ParseConfiguration -> Parser [Core]
lines conf =
  many endOfLine *> many (line conf <* many1 endOfLine <* option () endOfInput)

line :: ParseConfiguration -> Parser Core
line conf =
  (   (TickRateLine <$> tick conf)
    <|> (RefLine <$> ref conf)
    <|> (MBLLine <$> mbl conf)
    )
    <?> "Core Lines"

data IntermediateMBL = IntermediateMBL { candidateMbls :: [CoreMBL], refs :: Map.Map ByteString ByteString, tickRate :: TickRate }
toIntermediate :: [Core] -> Either String IntermediateMBL
toIntermediate mblLines = IntermediateMBL <$> mbls' <*> refs' <*> tickRate'
 where
  mbls'     = pure $ ([ x | (MBLLine x) <- mblLines ])
  refs'     = pure $ Map.fromList [ x | (RefLine x) <- mblLines ]
  tickRate' = case [ x | (TickRateLine x) <- mblLines ] of
    [a] -> Right a
    []  -> Right def
    _   -> Left "More than one tick rate is not allowed."

bindRefs :: IntermediateMBL -> [MBL]
bindRefs intermediate = bindCore <$> candidateMbls intermediate
 where
  bindCore :: CoreMBL -> MBL
  bindCore core = MBL
    { name    = cName core
    , actions = removeTrailingPrint $ concat $ bindAction <$> (cActions core)
    , repeat  = cRepeat core
    }
  bindAction :: CoreAction -> [Action]
  bindAction action = case action of
    CorePrint candidate -> intersperse
      (Wait { implicit = True, time = time' })
      (partialPrint candidate)
    CoreWait imp -> [Wait { implicit = imp, time = time' }]
  time' = D.toMicroseconds $ unTickRate (tickRate intermediate)
  partialPrint :: ByteString -> [Action]
  partialPrint candidate =
    case lookupBestMatch candidate (refs intermediate) of
      Nothing                  -> [Print candidate]
      Just (remainder, reffed) -> if BS.null remainder
        then [Print reffed]
        else Print reffed : partialPrint remainder

removeTrailingPrint :: [Action] -> [Action]
removeTrailingPrint a = case reverse a of
  Wait True _ : ac@(Print _) : acs -> reverse $ ac : acs
  _ -> a

lookupBestMatch :: ByteString -> Map.Map ByteString a -> Maybe (ByteString, a)
lookupBestMatch x = lookupBestMatch' BS.empty x

lookupBestMatch'
  :: ByteString -> ByteString -> Map.Map ByteString a -> Maybe (ByteString, a)
lookupBestMatch' acc key haystack = do
  (k, ks) <- BS.unsnoc key
  case Map.lookup key haystack of
    Just search -> Just (acc, search)
    Nothing     -> lookupBestMatch' (BS.cons ks acc) k haystack


ref :: ParseConfiguration -> Parser (ByteString, ByteString)
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

print :: ParseConfiguration -> Parser [CoreAction]
print conf =
  (:)
    <$> onePrint conf
    <*> (char (split conf) *> print conf <|> pure
          [CoreWait { cImplicit = True }]
        )
    <?> "Print"

onePrint :: ParseConfiguration -> Parser CoreAction
onePrint conf =
  CorePrint
    <$> unescape
    <$> takeWhile1_ (not . end)
    <*  skipWhile isSplit
    <?> "One Print"
 where
  isSplit c = c == split conf
  end c = c == delim || isEndOfLine c || isSplit c
  delim = delimiter conf

wait :: ParseConfiguration -> Parser [CoreAction]
wait conf = char delim *> pure ([CoreWait { cImplicit = False }]) <?> "Wait"
  where delim = delimiter conf


tick :: ParseConfiguration -> Parser TickRate
tick _ =
  TickRate
    <$> (string "tick" *> skipSpace *> char ':' *> skipSpace *> D.duration)
    <?> "One Ref line"


mbl :: ParseConfiguration -> Parser CoreMBL
mbl conf =
  CoreMBL
    <$> maybeOption name'
    <*  skipSpace
    <*> repeatWithDefault
    <*  skipSpace
    <*> (concat <$> many1 (wait conf <|> print conf))
    <?> "One MBL line"

repeatWithDefault :: Parser Repeat
repeatWithDefault = repeat' <|> pure Once

repeat' :: Parser Repeat
repeat' = (char '|' *> pure Infinite) <|> (Repeat <$> looping)
 where
  looping :: Parser Int
  looping =
    char '>' *> (round <$> scientific) <|> (length <$> many1 (char '>'))

parseRepeat :: String -> Either String Repeat
parseRepeat = parseOnly repeat' . BS.pack


name' :: Parser Name
name' = strip <$> takeWhile1_ (not . end) <* char ':' <* skipSpace
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


liftEither :: (MonadFail m, Show a) => Either a b -> m b
liftEither = either (\err -> fail $ show err) pure
