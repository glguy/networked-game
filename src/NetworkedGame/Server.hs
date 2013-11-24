module NetworkedGame.Server
  (NetworkServer(..), Handles, ConnectionId,

   serverMain, announce, announceOne
  )
  where

import Control.Concurrent (forkIO, threadDelay, ThreadId,
                           Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, handle, bracket_)
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Binary (Binary, encode, decode)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Network (PortID, accept, listenOn, Socket)
import Network.Socket (getSocketName)
import System.IO (Handle)
import qualified Data.ByteString.Lazy as B

import NetworkedGame.Handles
import NetworkedGame.Packet

data NetworkServer c w = NetworkServer
  { serverPort   :: PortID
  , eventsPerSecond :: Int
  , onTick       :: Handles -> Float             -> w -> IO w
  , onConnect    :: Handles -> ConnectionId      -> w -> IO w
  , onDisconnect :: Handles -> ConnectionId      -> w -> IO w
  , onCommand    :: Handles -> ConnectionId -> c -> w -> IO w
  }

-- | Main entry point for server
serverMain ::
  Binary c =>
  NetworkServer c w {- ^ callbacks and settings -} ->
  w                 {- ^ initial state value    -} ->
  IO ()
serverMain env w =
  do events             <- newChan
     _acceptThreadId    <- startNetwork env events
     lastTick           <- getCurrentTime
     _tickThreadId      <- forkIO $ tickThread env events
     eventLoop env events emptyHandles w lastTick

-- | Create a thread which will accept new connections.
-- Connections and disconnections will be announced to the event channel.
startNetwork ::
  Binary c =>
  NetworkServer c w ->
  Chan (ServerEvent c) ->
  IO ThreadId
startNetwork env events =
  do sock       <- listenOn $ serverPort env
     sockName   <- getSocketName sock
     putStrLn $ "Server listening on " ++ show sockName
     forkIO $ mapM_ (acceptClient events sock . ConnectionId) [0..]

-- | Accept a connection and create a thread to manage incoming data
-- from that connection.
acceptClient ::
  Binary c => 
  Chan (ServerEvent c) {- ^ channel to report connects to -} ->
  Socket       {- ^ listening socket             -} ->
  ConnectionId {- ^ next available connection id -} ->
  IO ThreadId
acceptClient events sock i =
  do (h,host,port) <- accept sock
     putStrLn $ concat ["Got connection from ", host, ":", show port]
     forkIO $ bracket_ (writeChan events $ JoinEvent i h)
                       (writeChan events $ DisconnectEvent i)
                       (clientSocketLoop i h events)

-- | Read incoming packets, decode them, and pass them along to
-- the event channel.
clientSocketLoop ::
  Binary c =>
  ConnectionId -> Handle -> Chan (ServerEvent c) -> IO ()
clientSocketLoop i h events =
  handle ignoreExceptions $
  forever $ do msg <- hGetPacketed h
               writeChan events $ ClientEvent i msg

-- | Send a command to a collection of clients
announce :: (MonadIO m, Binary c) =>
  Handles {- ^ Handles to send to -} ->
  c       {- ^ Message to send    -} ->
  m ()
announce hs msg = liftIO $
  do let p = mkPacket msg
     forHandles_ hs $ \h ->
       handle ignoreExceptions $
       hPutPacket h p

-- | Send a command to a single client identified by id.
announceOne ::
  (MonadIO m, Binary c) =>
  Handles      {- ^ Current set of handles     -} ->
  ConnectionId {- ^ Index of handle to send to -} ->
  c            {- ^ Message to send            -} ->
  m ()
announceOne hs i msg = liftIO $
  let p = mkPacket msg in
  for_ (lookupHandle i hs) $ \h ->
  handle ignoreExceptions  $
  hPutPacket h p

-- | Ignore all exceptions
ignoreExceptions :: SomeException -> IO ()
ignoreExceptions _ = return ()

-- | Thread which periodically enqueues tick events
tickThread ::
  NetworkServer c w    {- ^ game settings and callbacks     	-} ->
  Chan (ServerEvent c) {- ^ outgoing tick queue             	-} ->
  IO ()
tickThread env events =
  when (eventsPerSecond env > 0) $
      forever $ do writeChan events TickEvent
                   threadDelay $ 1000000 `div` eventsPerSecond env

data ServerEvent c
  = TickEvent
  | JoinEvent       ConnectionId Handle
  | DisconnectEvent ConnectionId
  | ClientEvent     ConnectionId c

eventLoop ::
  NetworkServer c w    {- ^ game settings and callbacks      -} ->
  Chan (ServerEvent c) {- ^ incoming event queue             -} ->
  Handles              {- ^ handles to current connections   -} ->
  w                    {- ^ initial state                    -} ->
  UTCTime              {- ^ time previous tick was processed -} ->
  IO ()
eventLoop env events = loop
  where
  loop hs w lastTick = do
     event <- readChan events
     case event of
       JoinEvent i h ->
         do let hs' = addHandle i h hs
            w' <- onConnect env hs' i w
            loop hs' w' lastTick

       TickEvent ->
         do now <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime now lastTick)
            w' <- onTick env hs elapsed w
            loop hs w' now

       ClientEvent i c ->
         do w' <- onCommand env hs i c w
            loop hs w' lastTick

       DisconnectEvent i ->
         do let hs' = removeHandle i hs
            w' <- onDisconnect env hs i w
            loop hs' w' lastTick
