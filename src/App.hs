{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module App where

import           Args                           ( args )
import           Mbl                            ( runParser
                                                , interpret
                                                , parseAll
                                                , MBL
                                                , name
                                                , Name
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

data Command = Hello MBL
             | TriggerStart
             | List
             | Update [MBL]
             deriving ( Generic, Show )


instance Serialize Command

data Client = Client { mbls :: MBL, channel :: Chan.Chan MBL }
newtype State = State { unClients :: [Client] }

data Response = Started Int
              | Start MBL
              | Listed [MBL]
              | Ok
                deriving ( Generic, Show )

{- 
  Command        | Response
  Hello          | await.... Start
  TriggerStart   | Started
  List           | Listed
  -> Update mbls | Ok
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
      Update newMBLs -> do
        lift $ Con.modifyMVar_ stateVar $ \state -> do
          clients <- updateMBLs newMBLs (unClients state)
          pure $ State clients
        P.yield $ Ok
      TriggerStart -> do
        clients <- lift $ unClients <$> Con.readMVar stateVar
        let n = length clients
        lift $ M.forM_ clients $ \client ->
          Chan.writeChan (channel client) (mbls client)
        P.yield $ Started n
        lift $ Con.modifyMVar_ stateVar $ \_ -> pure $ emptyState
      Hello mbl -> do
        newChan <- lift Chan.newChan
        clients <- lift $ unClients <$> Con.readMVar stateVar
        let otherMbls = mbls <$> clients
        mblWithName <- lift $ withName otherMbls mbl
        let newClient = Client mblWithName newChan
        lift $ Con.modifyMVar_ stateVar $ \state ->
          pure $ state { unClients = unClients state <> [newClient] }
        newMbl <- lift $ Chan.readChan newChan
        P.yield $ Start newMbl

withName :: [MBL] -> MBL -> IO MBL
withName mbls' mb = case name mb of
  Just name' -> case find (\m -> (name m) == Just name') mbls' of
    Just _ -> withName mbls' (mb { name = Just (name' <> "'") })
    _      -> pure mb
  Nothing -> do
    i <- R.randomRIO (0, length possibleNames - 1)
    let newName = possibleNames !! i
    withName mbls' (mb { name = Just newName })

possibleNames :: [Name]
possibleNames =
  [ "Big Pearl"
  , "Black Knight"
  , "Blazing Fireball"
  , "Blizzard Blaster"
  , "Blue Moon"
  , "Cobra"
  , "Comet"
  , "Cool Moody"
  , "Crazy Cat's Eye"
  , "Deep Ocean"
  , "Dragon's Egg"
  , "Ducktape"
  , "El Capitan"
  , "Ghost Plasma"
  , "Grasshopper"
  , "Green Turtle"
  , "H2 Blue"
  , "Lollipop"
  , "Marbly McMarbleface"
  , "Nemo"
  , "Phoenix"
  , "Pollo Loco"
  , "Quicksilver"
  , "Rastafarian"
  , "Red Number 3"
  , "Reflektor"
  , "Silver Bolt"
  , "Slimer"
  , "Summer Sky"
  , "Superball"
  , "White Widow"
  , "Wisp of Darkness"
  ]


updateMBLs :: [MBL] -> [Client] -> IO [Client]
updateMBLs newMbls cls = M.forM cls update
 where
  update :: Client -> IO Client
  update cl = case find (\m -> (name m) == needle) newMbls of
    Just new -> pure cl { mbls = new }
    Nothing ->
      fail $ "Can't update MBL with name " ++ maybe "-" BS.unpack needle
    where needle = (name $ mbls cl)

getContent :: C.Source -> IO BS.ByteString
getContent s = case s of
  C.File   filename -> BS.readFile $ BS.unpack filename
  C.Inline content  -> pure $ content


main :: IO ()
main = do
  config <- args
  case config of
    C.Inspect (C.InspectConfiguration (C.RunConfiguration source parseConfig))
      -> do
        contents <- getContent source
        let parsed = parseAll parseConfig contents
        either (\e -> fail $ "could not inspect this because " <> e)
               (print)
               parsed
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
      res <- D.runClient (C.unHost host) port command
      case (res :: Maybe Response) of
        Just (Listed mbls') -> do
          newContents <- E.runUserEditorDWIM (E.mkTemplate "mbl")
                                             (BS.pack $ show mbls')
          newMbls <-
            either (\e -> fail $ "could not inspect this because " <> e) (pure)
              $ parseAll
                  (C.ParseConfiguration '-' undefined Nothing Nothing Nothing) -- TODO: delimiter is BS. Undefined and general badness
                  newContents
          res2 <- D.runClient (C.unHost host) port (Update newMbls)
          case (res2 :: Maybe Response) of
            Just Ok -> pure ()
            _       -> fail $ "Unexpected response: " ++ show res2
        Just (Started count) -> print $ "Started " <> show count
        _                    -> fail $ "Unexpected response: " ++ show res

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

