{-|

The SSH Transport layer (RFC-4253). This module implements the
transport layer of ssh.

-}

{-# LANGUAGE OverloadedStrings  #-}
module Raaz.Network.SSH.Transport
       ( idString
       , idString'
       , receivePayload
       ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad                   (when)
import qualified Data.ByteString.Char8           as C8
import           Data.ByteString                 (ByteString)
import           Data.Version                    (showVersion)

import           Raaz.Memory
import           Raaz.Parse                      (runParser)
import           Raaz.Primitives
import           Raaz.Primitives.Cipher
import           Raaz.Types
import           Raaz.Util.ByteString
import           Raaz.Util.Ptr

import           Paths_raaz_ssh                  (version)
import           Raaz.Network.SSH.Backend
import           Raaz.Network.SSH.Context        (Context(..))
import           Raaz.Network.SSH.Exception
import           Raaz.Network.SSH.Message.Parser

-- | The prefix of the identification string.
idPrefix = C8.concat [ "SSH-2.0-"
                     , "RaazSSH_"
                     , C8.pack $ showVersion version
                     ]

-- | The identification string sent by the library.
idString :: C8.ByteString
idString = idPrefix `C8.append` "\r\n"

-- | The identification string with an extra comment field.
idString' :: C8.ByteString -> C8.ByteString
idString' comment = C8.concat [ idPrefix, " ", comment, "\r\n"]


-- | Performs the MAC verification and decryption and returns the
-- payload as a `ByteString`.
receivePayload :: Context -> IO ByteString
receivePayload (Context buff sock _ decr _) = withBuffer buff process
  where
    process buffptr = do
      (packLen,padLen) <- decrypt decr buffptr
      ok <- verify buffptr
      if ok
        then createFrom (packLen - padLen - 1) $ (buffptr `movePtr` headerSize)
        else throwIO $ TransportE MACFailure
    -- Verify MAC
    verify = undefined
    -- Decrypt and return (packet length, padding length)
    decrypt :: Gadget g => Maybe g -> CryptoPtr -> IO (BYTES Int,BYTES Int)
    decrypt Nothing buffptr = do
      receive sock headerSize buffptr
      (packLen,padLen) <- runParser buffptr $ (,) <$> (fromIntegral <$> parseWord32BE)
                                                  <*> (fromIntegral <$> parseByte)
      receive sock (packLen - 1 + macLen) buffptr
      return (packLen,padLen)
    decrypt (Just g) buffptr = do
      receive sock nBytes buffptr
      apply g nBlocks buffptr
      (packLen,padLen) <- runParser buffptr $ (,) <$> (fromIntegral <$> parseWord32BE)
                                                  <*> (fromIntegral <$> parseByte)
      let movedPtr = buffptr `movePtr` nBytes
      receive sock (packLen - 1 + macLen) buffptr
      apply g (cryptoCoerce $ packLen + nBytes - 4) movedPtr
      return (packLen,padLen)
      where
        singleBlock = 1
        (q,r) = headerSize `quotRem` cryptoCoerce singleBlock
        -- Ceil header size to cipher block size
        nBlocks = if r == 0 then fromIntegral q else fromIntegral q + singleBlock
        nBytes = fromIntegral nBlocks :: BYTES Int
    headerSize = 5 :: BYTES Int -- Cipher must decrypt atleast 5 bytes to calculate packet and padding length
    macLen = undefined

sendPayload :: Context -> ByteString -> IO ()
sendPayload (Context buff sock encr _ _) = undefined
