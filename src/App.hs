{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module App where

import           Args                           ( args )
import           Mbl                            ( runParser
                                                , interpret
                                                , parseAll
                                                , MBL
                                                , name
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
import           Data.List                      ( find )
import           System.Random                 as R
import           Paths_marble_os                ( version )
import           Data.Version                   ( showVersion )
import qualified System.Exit                   as Exit
import           Control.Exception              ( catch
                                                , IOException
                                                )
import qualified Control.Monad.Trans.Except    as TE

data Command = Hello MBL
             | TriggerStart
             | List
             | Update [MBL]
             | Version
             | Kill
             deriving ( Generic, Show )


instance Serialize Command

data Client = Client { mbls :: MBL, channel :: Chan.Chan MBL }
newtype State = State { unClients :: [Client] }

data Response = Started Int
              | Start MBL
              | Listed [MBL]
              | Ok
              | Error String
              | Versioned String
                deriving ( Generic, Show )

{- 
  Command        | Response
  Hello          | await.... Start
  TriggerStart   | Started
  List           | Listed
  -> Update mbls | Ok or Error
  Version        | Versioned
  Kill           | [dead]
 -}

instance Serialize Response

emptyState :: State
emptyState = State []

loop :: (Monad m) => TE.ExceptT e m a -> m e
loop = M.liftM (either id id) . TE.runExceptT . M.forever

quit :: (Monad m) => e -> TE.ExceptT e m r
quit = TE.throwE

handleCommands :: Con.MVar State -> DP.Handler ()
handleCommands stateVar reader writer =
  P.runEffect
    $   writer
    <-< serializer
    <-< commandExecuter
    <-< deserializer
    <-< reader
 where
  commandExecuter = loop $ do
    command <- lift $ P.await
    case command of
      Version -> do
        lift $ P.yield $ Versioned $ showVersion version
      Kill -> do
        lift $ P.yield $ Ok
        quit ()
      List -> do
        clients <- lift $ lift $ Con.readMVar stateVar
        let mbls' = mbls <$> unClients clients
        lift $ P.yield $ Listed mbls'
      Update newMBLs -> do
        opSuccess <- lift $ lift $ Con.modifyMVar stateVar $ \state -> do
          let clients = TE.runExcept $ updateMBLs newMBLs (unClients state)
          pure $ (either (const state) State clients, clients)
        lift $ P.yield $ either Error (const Ok) opSuccess
      TriggerStart -> do
        clients <- lift $ lift $ unClients <$> Con.readMVar stateVar
        let n = length clients
        lift $ lift $ M.forM_ clients $ \client ->
          Chan.writeChan (channel client) (mbls client)
        lift $ P.yield $ Started n
        quit ()
      Hello mbl -> do
        newChan <- lift $ lift Chan.newChan
        clients <- lift $ lift $ unClients <$> Con.readMVar stateVar
        let otherMbls = mbls <$> clients
        mblWithName <- lift $ lift $ withName otherMbls mbl
        let newClient = Client mblWithName newChan
        lift $ lift $ Con.modifyMVar_ stateVar $ \state ->
          pure $ state { unClients = unClients state <> [newClient] }
        newMbl <- lift $ lift $ Chan.readChan newChan
        lift $ P.yield $ Start newMbl

withName :: [MBL] -> MBL -> IO MBL
withName mbls' mb = case name mb of
  Just name' -> case find (\m -> (name m) == Just name') mbls' of
    Just _ -> withName mbls' (mb { name = Just (name' <> "'") })
    _      -> pure mb
  Nothing -> do
    i <- R.randomRIO (0, length C.possibleNames - 1)
    let newName = C.possibleNames !! i
    withName mbls' (mb { name = Just $ BS.pack newName })


updateMBLs :: [MBL] -> [Client] -> TE.Except String [Client]
updateMBLs newMbls cls = M.forM cls update
 where
  update :: Client -> TE.Except String Client
  update cl = case find (\m -> (name m) == needle) newMbls of
    Just new -> pure cl { mbls = new }
    Nothing ->
      TE.throwE $ "Can't update MBL with name " ++ maybe "-" BS.unpack needle
    where needle = (name $ mbls cl)

getContent :: C.Source -> IO BS.ByteString
getContent s = case s of
  C.File   filename -> BS.readFile $ BS.unpack filename
  C.Inline content  -> pure $ content


main :: IO ()
main = do
  config <- args
  case config of
    C.Version (C.VersionConfiguration remote) -> do
      let port = C.port remote
      let host = C.host remote
      res <- D.runClient (C.unHost host) port Version
        `catch` \(_ :: IOException) -> pure Nothing
      putStrLn ("marble (local)  " ++ showVersion version)
      case res of
        Just (Versioned v) -> putStrLn ("marble (daemon) " ++ v)
        Nothing            -> putStrLn "marble (daemon) not started"
        _                  -> fail $ "Unexpected response: " ++ show res
    C.Inspect (C.InspectConfiguration (C.RunConfiguration source parseConfig))
      -> do
        contents <- getContent source
        let parsed = runParser parseConfig contents
        either (\e -> fail $ "could not inspect this because " <> e)
               (putStrLn . show)
               parsed
    C.Run (C.RunConfiguration source parseConfig) -> do
      contents <- getContent source
      mbl      <- either (fail) pure $ runParser parseConfig contents
      interpret mbl
    C.Daemon (C.DaemonConfiguration config' remote) -> do
      let port    = C.port remote
      let host    = C.host remote
      let options = def { D.daemonPort = port } -- TODO duplicated!
      if host == def
        then do
          state <- Con.newMVar emptyState
          D.ensureDaemonWithHandlerRunning "marble-os"
                                           options
                                           (handleCommands state)
        else pure ()
      res <- D.runClient (C.unHost host) port command
      case (config', res :: Maybe Response) of
        (C.Edit, Just (Listed mbls')) -> do
          newContents <- E.runUserEditorDWIM (E.mkTemplate "mbl")
                                             (BS.pack $ show mbls')
          newMbls <-
            either (\e -> fail $ "could not inspect this because " <> e) (pure)
              $ parseAll
                  (C.ParseConfiguration '-' undefined Nothing Nothing Nothing) -- TODO: delimiter is BS. Undefined and general badness
                  newContents
          res2 <- D.runClient (C.unHost host) port (Update newMbls)
          case (res2 :: Maybe Response) of
            Just Ok          -> pure ()
            Just (Error err) -> fail err
            _                -> fail $ "Unexpected response: " ++ show res2
        (C.List, Just (Listed mbls')) -> do
          putStrLn $ show mbls'
        (C.Start, Just (Started count)) -> putStrLn $ "Started " <> show count
        (_      , Just Ok             ) -> putStrLn "ok"
        (_      , Nothing             ) -> Exit.exitFailure
        _ ->
          fail $ "Unexpected response: " ++ show res ++ "for: " ++ show config'
     where
      command :: Command
      command = case config' of
        C.List  -> List
        C.Edit  -> List
        C.Start -> TriggerStart
        C.Kill  -> Kill
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

