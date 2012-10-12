module NetworkedGame.Packet where

import qualified Data.ByteString.Lazy as B
import Data.Binary (Binary,encode,decode)
import Data.Int (Int64)
import System.IO (Handle, hFlush)

newtype Packet = Packet B.ByteString

mkPacket :: Binary a => a -> Packet
mkPacket x = Packet $ B.append (encode n) bs
  where
  n = B.length bs
  bs = encode x

hPutPacket :: Handle -> Packet -> IO ()
hPutPacket h (Packet bs) = B.hPutStr h bs >> hFlush h

hGetPacketed :: Binary a => Handle -> IO a
hGetPacketed h =
  do nEnc <- B.hGet h 8
     let n :: Int64
         n = decode nEnc
     bs  <- B.hGet h $ fromIntegral n
     return $ decode bs
