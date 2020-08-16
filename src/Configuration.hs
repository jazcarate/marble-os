module Configuration where

import qualified Data.Text                     as T
import qualified Duration                      as D

type Delimiter = Char

data Configuration = Configuration
  { path        :: T.Text
  , lane        :: Int
  , repeat      :: Bool
  , tick        :: D.Duration
  , delimiter   :: Delimiter }
