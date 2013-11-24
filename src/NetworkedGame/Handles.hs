module NetworkedGame.Handles
 (ConnectionId(..), Handles,
  emptyHandles, removeHandle, lookupHandle,
  nullHandles, addHandle, forHandles_)
 where

import Data.Foldable (forM_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.IO (Handle)

newtype Handles = Handles (IntMap Handle)

newtype ConnectionId = ConnectionId Int
  deriving (Eq, Show, Ord, Read)

emptyHandles :: Handles
emptyHandles = Handles IntMap.empty

addHandle :: ConnectionId -> Handle -> Handles -> Handles
addHandle (ConnectionId i) h (Handles hs) = Handles (IntMap.insert i h hs)

removeHandle :: ConnectionId -> Handles -> Handles
removeHandle (ConnectionId i) (Handles hs) = Handles (IntMap.delete i hs)

lookupHandle :: ConnectionId -> Handles -> Maybe Handle
lookupHandle (ConnectionId i) (Handles m) = IntMap.lookup i m

nullHandles :: Handles -> Bool
nullHandles (Handles m) = IntMap.null m

forHandles_ :: Monad m => Handles -> (Handle -> m a) -> m ()
forHandles_ (Handles m) f = forM_ m f
