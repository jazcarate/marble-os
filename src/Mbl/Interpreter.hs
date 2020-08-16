module Mbl.Interpreter where

import           Prelude                 hiding ( repeat )
import           Mbl.Types
import qualified Control.Concurrent            as C
import qualified Control.Monad                 as CM
import qualified Data.ByteString.Char8         as BS
import qualified Configuration                 as C
import qualified Duration                      as D
import qualified Data.Bool                     as B

interpret :: C.Configuration -> MBL -> IO ()
interpret config mlb = CM.forM_ (repeat mlb) interpret'
 where
  repeat :: MBL -> MBL
  repeat = B.bool id cycle (C.repeat config)
  interpret' :: Action -> IO ()
  interpret' Wait        = C.threadDelay $ D.toMicroseconds $ C.tick config
  interpret' (Print str) = BS.putStrLn str
