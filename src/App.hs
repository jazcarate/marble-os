{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module App where

import           Args                           ( args )
import           Mbl                            ( runParser
                                                , interpret
                                                , MBL
                                                )
import qualified Data.ByteString.Char8         as BS
import qualified Configuration                 as C
import qualified Control.Concurrent.MVar       as Con
import qualified System.Daemon                 as D
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
import qualified Text.Editor                   as E

data Command = Hello MBL
             | TriggerStart
             | List
             deriving ( Generic, Show )


instance Serialize Command

data Client = Client { mbls :: MBL, channel :: Chan.Chan MBL }
newtype State = State { unClients :: [Client] }

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

emptyState :: State
emptyState = State []

handleCommands :: Con.MVar State -> DP.Handler ()
handleCommands stateVar reader writer =
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
        clients <- lift $ Con.readMVar stateVar
        let mbls' = mbls <$> unClients clients
        P.yield $ Listed mbls'
      TriggerStart -> do
        clients <- lift $ unClients <$> Con.readMVar stateVar
        let n = length clients
        lift $ M.forM_ clients $ \client ->
          Chan.writeChan (channel client) (mbls client)
        P.yield $ Started n
        lift $ Con.modifyMVar_ stateVar $ \_ -> pure $ emptyState
      Hello mbl -> do
        newChan <- lift Chan.newChan
        let newClient = Client mbl newChan
        lift $ Con.modifyMVar_ stateVar $ \state ->
          pure $ state { unClients = unClients state <> [newClient] }
        newMbl <- lift $ Chan.readChan newChan
        P.yield $ Start newMbl

getContent :: C.Source -> IO BS.ByteString
getContent s = case s of
  C.File   filename -> BS.readFile $ BS.unpack filename
  C.Inline content  -> pure $ content


main :: IO ()
main = do
  config <- args
  case config of
    C.Run (C.RunConfiguration source parseConfig) -> do
      contents <- getContent source
      mbl      <- either (fail) pure $ runParser parseConfig contents
      interpret mbl
    C.Daemon (C.DaemonConfiguration config' remote) -> do
      let port    = C.port remote
      let host    = C.host remote
      let options = def { D.daemonPort = port, D.printOnDaemonStarted = False } -- TODO duplicated!
      if host == def
        then do
          state <- Con.newMVar emptyState
          D.ensureDaemonWithHandlerRunning "marble-os"
                                           options
                                           (handleCommands state)
        else pure ()
      res <- D.runClient (C.unHost host) port (command)
      print (res :: Maybe Response)
      x <- E.runUserEditorDWIM (E.mkTemplate "mbl") (BS.pack $ show res) -- TODO: I left off here
      print x

     where
      command :: Command
      command = case config' of
        C.List  -> List
        C.Start -> TriggerStart
    C.Sync (C.SyncConfiguration (C.RunConfiguration source parseConfig) remote)
      -> do
        let port = C.port remote
        let host = (C.unHost $ C.host remote)
        let options =
              def { D.daemonPort = port, D.printOnDaemonStarted = False } -- TODO duplicated!
        if host == def
          then do
            state <- Con.newMVar emptyState
            D.ensureDaemonWithHandlerRunning "marble-os"
                                             options
                                             (handleCommands state)
          else pure ()
        contents <- getContent source
        mbl      <- either (fail) pure $ runParser parseConfig contents
        res      <- D.runClient host port (Hello mbl)
        case res of
          Just (Start newMbl) -> interpret newMbl
          _                   -> fail $ show res

