module App where

import           Args                           ( args )
import           Mbl                            ( parse
                                                , interpret
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Configuration                 as C

main :: IO ()
main = do
  config   <- args
  contents <- BS.readFile $ T.unpack $ C.path config
  mbl      <- either (fail) pure $ parse config contents
  interpret config mbl
