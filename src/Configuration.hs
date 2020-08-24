module Configuration where

import qualified Data.Text                     as T
import qualified Duration                      as D
import           Data.Default                   ( Default(..) )

type Delimiter = Char

data RunConfiguration = RunConfiguration
  { path        :: T.Text
  , lane        :: Lane
  , repeat      :: Bool
  , tick        :: D.Duration
  , delimiter   :: Delimiter }


data SyncConfiguration = SyncConfiguration RunConfiguration Remote

data DaemonConfiguration = DaemonConfiguration DaemonSubConfiguration Remote

data DaemonSubConfiguration = List | Start

data Lane = Numbered Int | Named T.Text

data Remote =  Remote { port   :: Port , host :: Host }


data Configuration = Run RunConfiguration | Sync SyncConfiguration | Daemon DaemonConfiguration

newtype Host = Host { unHost :: String } deriving (Show, Eq, Read)

type Port = Int

instance Default Host where
  def = Host "127.0.0.1"
