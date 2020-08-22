module Configuration where

import qualified Data.Text                     as T
import qualified Duration                      as D
import           Data.Default                   ( Default(..) )

type Delimiter = Char

data RunConfiguration = RunConfiguration
  { path        :: T.Text
  , lane        :: Int
  , repeat      :: Bool
  , tick        :: D.Duration
  , delimiter   :: Delimiter }


data SyncConfiguration = SyncConfiguration RunConfiguration Remote

data DaemonConfiguration = DaemonConfiguration DaemonSubConfiguration Remote

data DaemonSubConfiguration = List | Start

data Remote =  Remote { port   :: Port , host :: Host }


data Configuration = Run RunConfiguration | Sync SyncConfiguration | Daemon DaemonConfiguration

newtype Host = Host { unHost :: T.Text } deriving (Show, Eq, Read)

type Port = Int

instance Default Host where
  def = Host "127.0.0.1"
