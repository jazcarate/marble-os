module Mbl where

import qualified Mbl.Types                     as Types
import qualified Mbl.Parser                    as Parser
import qualified Mbl.Interpreter               as Interpreter
import qualified Data.ByteString               as BS
import qualified Configuration                 as C

parseMbl :: C.Configuration -> BS.ByteString -> Either String Types.MBL
parseMbl = Parser.parse

interpret :: C.Configuration -> Types.MBL -> IO ()
interpret = Interpreter.interpret
