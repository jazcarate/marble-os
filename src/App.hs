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
import qualified Control.Pipe.Socket           as DP
import qualified Control.Monad                 as M
import           Control.Monad.Trans.Class      ( lift )
import           Control.Pipe.Serialize         ( serializer
                                                , deserializer
                                                )
import qualified Control.Concurrent.Chan       as Chan

data Command = Hello MBL
             | TriggerStart
             | List
             deriving ( Generic, Show )


instance Serialize Command

type Registry = Cursor.ListCursor (MBL, Chan.Chan MBL)

data Response = Started Int
              | Start MBL
              | Listed [MBL]
                deriving ( Generic, Show )

{- 
  Command      | Response
  Hello        | await.... Start
  TriggerStart | Started
  List         | Listed
 -}

instance Serialize Response

handleCommands :: Con.MVar Registry -> DP.Handler ()
handleCommands registryVar reader writer =
  P.runEffect
    $   writer
    <-< serializer
    <-< commandExecuter
    <-< deserializer
    <-< reader
 where
  commandExecuter = M.forever $ do
    command <- P.await
    case command of
      List -> do
        registry <- lift $ Con.readMVar registryVar
        let mbls = fst <$> Cursor.rebuildListCursor registry
        P.yield $ Listed mbls
      TriggerStart -> do
        registry <- lift $ Con.readMVar registryVar
        let n = Cursor.listCursorLength registry
        lift $ M.forM_ (Cursor.rebuildListCursor registry) $ \reg ->
          Chan.writeChan (snd reg) (fst reg)
        P.yield $ Started n
        lift $ Con.modifyMVar_ registryVar $ \_ -> pure $ Cursor.emptyListCursor
      Hello mbl -> do
        newChan <- lift Chan.newChan
        let newReg = (mbl, newChan)
        lift $ Con.modifyMVar_ registryVar $ \reg ->
          pure $ Cursor.listCursorAppend newReg reg
        newMbl <- lift $ Chan.readChan newChan
        P.yield $ Start newMbl


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
      let options = def { D.daemonPort = port, D.printOnDaemonStarted = False } -- TODO duplicated!
      if host == def
        then do
          state <- Con.newMVar Cursor.emptyListCursor
          D.ensureDaemonWithHandlerRunning "marble-os"
                                           options
                                           (handleCommands state)
        else pure ()
      res <- D.runClient (T.unpack $ C.unHost host) port (command)
      print (res :: Maybe Response)
     where
      command :: Command
      command = case config' of
        C.List  -> List
        C.Start -> TriggerStart
    C.Sync (C.SyncConfiguration config' remote) -> do
      let port    = C.port remote
      let host    = (T.unpack $ C.unHost $ C.host remote)
      let options = def { D.daemonPort = port, D.printOnDaemonStarted = False } -- TODO duplicated!
      if host == def
        then do
          state <- Con.newMVar Cursor.emptyListCursor
          D.ensureDaemonWithHandlerRunning "marble-os"
                                           options
                                           (handleCommands state)
        else pure ()
      contents <- BS.readFile $ T.unpack $ C.path config'
      mbl      <- either (fail) pure $ parse config' contents
      res      <- D.runClient host port (Hello mbl)
      case res of
        Just (Start newMbl) -> interpret config' newMbl
        _                   -> fail "TODO"

