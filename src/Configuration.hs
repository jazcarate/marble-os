{-# LANGUAGE DeriveGeneric #-}

module Configuration where

import qualified Duration                      as D
import           Data.Default                   ( Default(..) )
import           Data.ByteString                ( ByteString )
import           GHC.Generics                   ( Generic )
import           Data.Serialize                 ( Serialize )

type Delimiter = Char

-- Source -> String
data Source = File ByteString | Inline ByteString

newtype TickRate = TickRate { unTickRate :: D.Duration }
-- String + ParseConfiguration = MBL
data ParseConfiguration = ParseConfiguration
  { delimiter       :: Delimiter
  , lane            :: Lane
  --Overrides
  , nameOverride            :: Maybe ByteString
  , tickRateOverride        :: Maybe TickRate
  , repeatStrategyOverride  :: Maybe Repeat }


data Repeat = Infinite | Repeat Int | Once deriving (Eq, Generic)
instance Serialize Repeat

instance Show Repeat where
  show c = case c of
    Infinite -> "|"
    Repeat i -> if i > 3 then ">" ++ show i else replicate i '>'
    Once     -> ""

data RunConfiguration = RunConfiguration Source ParseConfiguration

data SyncConfiguration = SyncConfiguration RunConfiguration Remote

data DaemonConfiguration = DaemonConfiguration DaemonSubConfiguration Remote

data InspectConfiguration = InspectConfiguration RunConfiguration

data DaemonSubConfiguration = List | Start

data Lane = Numbered Int | Named ByteString

data Remote =  Remote { port   :: Port , host :: Host }

data Configuration = Run RunConfiguration | Sync SyncConfiguration | Daemon DaemonConfiguration | Inspect InspectConfiguration

newtype Host = Host { unHost :: String } deriving (Show, Eq, Read)

type Port = Int

instance Default Host where
  def = Host "127.0.0.1"

instance Default TickRate where
  def = TickRate $ D.seconds 1
