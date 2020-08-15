module App where

import           Args                           ( args
                                                , path
                                                )
import           Mbl                            ( parseMbl, interpret )
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T

main :: IO ()
main = do
  config   <- args
  contents <- BS.readFile $ T.unpack $ path config
  mbl <- either (fail) pure $ parseMbl contents
  interpret mbl
  putStrLn "3"
  putStrLn "4"
