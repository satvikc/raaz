{-# LANGUAGE DeriveDataTypeable #-}
module Raaz.Network.SSH.Exception where

import Control.Exception
import Data.Typeable

data SSHException = TransportE TransportException deriving (Show,Typeable)

instance Exception SSHException

data TransportException = MACFailure
                        | IllegalPacket
                        | PacketTooLarge deriving (Show,Typeable)
