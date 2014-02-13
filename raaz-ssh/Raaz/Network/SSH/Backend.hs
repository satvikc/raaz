{-# LANGUAGE BangPatterns #-}
module Raaz.Network.SSH.Backend where

import Control.Monad       (when,void)
import Foreign.Ptr         (castPtr, plusPtr)
import Network.Socket      (Socket, sClose, sendBuf, recvBuf)
import System.IO           (Handle,hClose,hPutBuf,hGetBuf)

import Raaz.Types


-- | Represents an SSH Backend used for buffered communication.
class Backend b where
  close   :: b -> IO ()
  send    :: b -> BYTES Int -> CryptoPtr -> IO ()
  receive :: b -> BYTES Int -> CryptoPtr -> IO ()

-- | Backend for network sockets. It should only be used for
-- Streaming Sockets over TCP.
instance Backend Socket where
  close = sClose
  send sock = send'
    where
      send' !(BYTES bytes) !cptr = do
        sent <- sendBuf sock (castPtr cptr) bytes
        when (sent < bytes) $ send' (BYTES $ bytes - sent) (cptr `plusPtr` sent)
  receive sock = receive'
    where
      receive' !(BYTES bytes) !cptr = do
        received <- recvBuf sock (castPtr cptr) bytes
        when (received < bytes) $ receive' (BYTES $ bytes - received)
                                           (cptr `plusPtr` received)

-- | Backend for File Handles.
instance Backend Handle where
  close = hClose
  send hndl (BYTES bytes) cptr = hPutBuf hndl (castPtr cptr) bytes
  receive hndl (BYTES bytes) cptr = void $ hGetBuf hndl (castPtr cptr) bytes
