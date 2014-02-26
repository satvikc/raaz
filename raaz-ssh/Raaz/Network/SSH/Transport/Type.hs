{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE Rank2Types         #-}

module Raaz.Network.SSH.Transport.Type where

import Data.Typeable
import Control.Applicative ((<$>))
import Control.Exception
import Raaz.Memory
import Raaz.Random
import Raaz.Types


import Raaz.Network.SSH.Backend


-- | It captures the connection state which includes the message
-- buffer, backend, payload handler and random source.
data Context where
  Context :: (Bufferable a,Backend b,PayloadHandler h)
          => Buffer a               -- ^ Buffer
          -> b                      -- ^ Backend
          -> h                      -- ^ Payload handler
          -> RandomSource r         -- ^ Random Number Generator
          -> Context

-- | Payload Handler handles the packet received from a connection. It
-- takes care of all the decompression, verfication and decryption
-- required.
class PayloadHandler h where
  -- | Payload handler has to fetch some initial data to decide on how
  -- much more data to fetch. For example, in case of a block
  -- encrypted packet it will fetch atleast a block to decrypt packet
  -- length and fetch appropriately more data from the backend.
  initialFetchSize :: h -> BYTES Int
  -- | After the buffer is filled with the required data from the
  -- backend this function calculates how much more data to fetch.
  initializeHandler :: h              -- ^ Handler
                    -> CryptoPtr      -- ^ Buffer Pointer
                    -> IO (BYTES Int) -- ^ Bytes more to fetch
  -- | This function is called when the requested amount of data
  -- is already fetched by the backend.
  finalizeHandler :: h          -- ^ Handler
                  -> BYTES Int  -- ^ Fetched data size
                  -> CryptoPtr  -- ^ Fetched data location
                  -> IO ()

-- | Handler for data which doesn't have any encryption, compression
-- or authentication.
data SimpleHandler = SimpleHandler

instance PayloadHandler SimpleHandler where
  initialFetchSize _ = 4
  initializeHandler _ cptr = BYTES . fromIntegral <$> (load cptr :: IO Word32BE)
  finalizeHandler _ _ _ = return ()
