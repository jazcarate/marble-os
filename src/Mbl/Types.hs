module Mbl.Types where


import           Data.ByteString                ( ByteString )

data Action = Wait | Print ByteString deriving (Show, Eq)

type MBL = [Action]
