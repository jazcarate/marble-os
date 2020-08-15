module Mbl where

import qualified Mbl.Types                     as Types
import qualified Mbl.Parser                    as Parser
import qualified Mbl.Interpreter               as Interpreter
import qualified Data.ByteString               as BS

parseMbl :: BS.ByteString -> Either String Types.MBL
parseMbl = Parser.parse

interpret :: Types.MBL -> IO ()
interpret = Interpreter.interpret
