module Mbl.Interpreter where

import           Mbl.Types
import qualified Control.Concurrent            as C


interpret :: MBL -> IO ()
interpret _ = C.threadDelay 30000000
