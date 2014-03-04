{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE BangPatterns       #-}

module Raaz.Network.SSH.Transport.Type where

import Data.Word
import Data.Typeable
import Control.Applicative      ((<$>))
import Control.Exception
import Foreign.Ptr              (castPtr)
import Foreign.Storable         (peek,poke)

import Raaz.Memory
import Raaz.Primitives
import Raaz.Primitives.Cipher
import Raaz.Random
import Raaz.Types
import Raaz.Util.Ptr


import Raaz.Network.SSH.Backend


-- | It captures the connection state which includes the message
-- buffer, backend, payload handler and random source.
data Context where
  Context :: (Bufferable a,Backend b,PacketProcessor h,StreamGadget r)
          => Buffer a               -- ^ Buffer
          -> b                      -- ^ Backend
          -> h                      -- ^ Payload handler
          -> RandomSource r         -- ^ Random Number Generator
          -> Context

-- | Payload data type which consist of payload size and location.
data Payload = Payload {-# UNPACK #-} !(BYTES Int)
                       {-# UNPACK #-} !CryptoPtr

-- | Applies the given function on `CryptoPtr` associated with `Payload`.
withPayload :: Payload -> (CryptoPtr -> BYTES Int -> IO b) -> IO b
withPayload (Payload sz cptr) f = f cptr sz

-- | Packet Processor handles the packet received from a
-- connection. It takes care of all the decompression, verfication and
-- decryption when receiving Payload and compression, mac generatation
-- and encryption while sending payload.
class PacketProcessor h where

  -- | it fetches and processes the first block and depending on the
  -- packet length processes the rest of the packet and returns
  -- payload.
  getPayload :: (Bufferable a, Backend b)
             => h          -- ^ Packet Processor
             -> b          -- ^ SSH Backend
             -> Buffer a   -- ^ Buffer
             -> IO Payload -- ^ Processed Payload

  -- | Given the function to write payload at specified location, it
  -- creates and sends the packet over the network backend.
  sendPayloadWith :: (Bufferable a, Backend b, StreamGadget r)
                  => h                             -- ^ Packet Processor
                  -> b                             -- ^ SSH Backend
                  -> Buffer a                      -- ^ Buffer
                  -> RandomSource r                -- ^ Random Source
                  -> (CryptoPtr -> IO (BYTES Int)) -- ^ Payload Writer
                  -> IO ()

-- | Handler for data which doesn't have any encryption, compression
-- or authentication.
data SimplePacket = SimplePacket

instance  PacketProcessor SimplePacket where
  getPayload _ back buff = withBuffer buff go
    where
      go cptr = do
        receive back (byte 4) cptr
        packLen <- BYTES . fromIntegral <$> (load cptr :: IO Word32BE)
        let movedPtr = cptr `movePtr` (byte 4)
        receive back packLen movedPtr
        pad <- peek (castPtr movedPtr) :: IO Word8
        let padsz = BYTES $ fromIntegral pad
        return $ Payload (packLen - padsz - byte 1) (cptr `movePtr` byte 1)
  sendPayloadWith _  back buff rsrc writePayload = withBuffer buff go
    where
      go cptr = do
        let payloadPtr = cptr `movePtr` byte 5
        sz <- writePayload payloadPtr
        store cptr (fromIntegral sz :: Word32BE)
        padLen <- genMax rsrc 255
        let padBytes = byte $ fromIntegral padLen
        poke (castPtr $ cptr `movePtr` byte 4) (padLen :: Word8)
        apply rsrc (cryptoCoerce padBytes) (payloadPtr `movePtr` sz)
        send back (byte 5 + sz + padBytes) cptr

byte :: Int -> BYTES Int
byte = BYTES
{-# INLINE byte #-}
