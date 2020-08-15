module App where

import           Args                           ( args )


main :: IO ()
main = do
  config <- args
  print config
  putStrLn "3"
  putStrLn "4"
