{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module App where

import           Args                           ( args )
import           Mbl                            ( parse
                                                , interpret
                                                , MBL
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Configuration                 as C
import qualified Control.Concurrent.MVar       as Con
import qualified System.Daemon                 as D
import qualified Cursor.List                   as Cursor
import           GHC.Generics
import           Data.Serialize                 ( Serialize )
import           Data.Default                   ( def )
import qualified Pipes                         as P
import           Pipes                          ( (<-<) )
import qualified Control.Pipe.Serialize        as P
import qualified Control.Monad                 as M
import           Control.Monad.Trans.Class      ( lift )

data Command = Hello MBL
             | Start
             | List
             deriving ( Generic, Show )


instance Serialize Command

type Registry = Cursor.ListCursor MBL

data Response = Ok
              | Listed [MBL]
                deriving ( Generic, Show )

instance Serialize Response

handleCommands :: Con.MVar Registry -> Command -> IO Response
handleCommands registryVar command = case command of
  Hello mbl -> Con.modifyMVar registryVar
    $ \registry -> return $ (Cursor.listCursorAppend mbl registry, Ok)
  Start -> error "can't start. Did not program it"
  List  -> Listed <$> Cursor.rebuildListCursor <$> Con.readMVar registryVar

main :: IO ()
main = do
  config <- args
  case config of
    C.Run config' -> do
      contents <- BS.readFile $ T.unpack $ C.path config'
      mbl      <- either (fail) pure $ parse config' contents
      interpret config' mbl
    C.Daemon (C.DaemonConfiguration config' remote) -> do
      let port    = C.port remote
      let host    = C.host remote
      let options = def { D.daemonPort = port, D.printOnDaemonStarted = False }
      state <- Con.newMVar Cursor.emptyListCursor
      if host == def
        then D.ensureDaemonRunning "marble-os" options (handleCommands state)
        else pure ()
      res <- D.runClient (T.unpack $ C.unHost host) port (command)
      print (res :: Maybe Response)
     where
      command :: Command
      command = case config' of
        C.List  -> List
        C.Start -> Start
    C.Sync (C.SyncConfiguration config' remote) -> do
      let port = C.port remote
      contents <- BS.readFile $ T.unpack $ C.path config'
      mbl      <- either (fail) pure $ parse config' contents
      D.runClientWithHandler (T.unpack $ C.unHost $ C.host remote) port
        $ \reader writer -> P.runEffect $ do
            writer <-< P.serializer <-< P.yield (Hello mbl)
            (M.forever $ P.await >>= \(res :: Maybe Response) ->
                lift (print (Just res))
              )
              <-< P.deserializer
              <-< reader

