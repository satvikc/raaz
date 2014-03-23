{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raaz.Network.SSH.Message.Write where

import           Control.Applicative
import           Control.Exception
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Data.Typeable
import           Data.Word


import           Raaz.WriteSafe
import           Raaz.Types

import           Raaz.Network.SSH.Message.Type

writeMessage :: Message -> Write
writeMessage (Disconnect reason description) =
  writeByte 1
writeMessage (Ignore msg) =
  writeByte 2 <> writeByteString msg
writeMessage (Unimplemented pack) =
  writeByte 3 <> write pack
writeMessage (Debug b desc) =
  writeByte 4 <> writeBool b <> writeDescription desc
writeMessage (ServiceRequest s) =
  writeByte 5 <> writeService s
writeMessage (ServiceAccept s) =
  writeByte 6 <> writeService s

writeMessage (Kexinit cookie ex host cipc2s cips2c macc2s
                      macs2c comc2s coms2c langc2s langs2c guessed) =
  writeByte 20 <> writeByteString cookie
               <> writeNameList ex
               <> writeNameList host
               <> writeNameList cipc2s
               <> writeNameList cips2c
               <> writeNameList macc2s
               <> writeNameList macs2c
               <> writeNameList comc2s
               <> writeNameList coms2c
               <> writeNameList langc2s
               <> writeNameList langs2c
               <> writeBool guessed

writeMessage Newkeys =
  writeByte 21

writeMessage (Request uname service method) =
  writeByte 50 <> writeText uname
               <> writeService service
               <> writeAuthMethod method

writeMessage (Failure continue partial) =
  writeByte 51 <> writeNameList continue
               <> writeBool partial

writeMessage Success =
  writeByte 52

writeMessage (Banner descr) =
  writeByte 53 <> writeDescription descr

writeMessage (GlobalRequest reply req) =
  writeByte 80 <> writeBool reply
               <> writeGlobalRequestType req

writeMessage (RequestSuccess succdata) =
  writeByte 81 <> writeByteString succdata

writeMessage RequestFailure =
  writeByte 82

writeMessage (ChannelOpen cid window packet ctype) =
  writeByte 90 <> write cid
               <> write window
               <> write packet
               <> writeChannelType ctype

writeMessage (ChannelOpenConfirmation cid senderid window packet confdata) =
  writeByte 91 <> write cid
               <> write senderid
               <> write window
               <> write packet
               <> writeByteString confdata

writeMessage (ChannelOpenFailure cid reason descr) =
  writeByte 92 <> write cid
               <> writeWord32BE (fromIntegral $ fromEnum reason)
               <> writeDescription descr

writeMessage (ChannelWindowAdjust cid size) =
  writeByte 93 <> write cid
               <> write size

writeMessage (ChannelData cid bs) =
  writeByte 94 <> write cid
               <> writeByteString bs

writeMessage (ChannelExtendedData cid exttype extdata) =
  writeByte 95 <> write cid
               <> writeExtendedDataType exttype
               <> writeByteString extdata

writeMessage (ChannelEOF cid) =
  writeByte 96 <> write cid

writeMessage (ChannelClose cid) =
  writeByte 97 <> write cid

writeMessage (ChannelRequest cid reqtype reply) =
  writeByte 98 <> write cid
               <> writeChannelRequestType reqtype
               <> writeBool reply

writeMessage (ChannelSuccess cid) =
  writeByte 99 <> write cid

writeMessage (ChannelFailure cid) =
  writeByte 100 <> write cid

writeService :: Service -> Write
writeService = undefined

writeAuthMethod :: AuthMethod -> Write
writeAuthMethod = undefined

writeGlobalRequestType :: GlobalRequestType -> Write
writeGlobalRequestType = undefined

writeExtendedDataType :: ExtendedDataType -> Write
writeExtendedDataType = undefined

writeChannelType :: ChannelType -> Write
writeChannelType = undefined

writeChannelRequestType :: ChannelRequestType -> Write
writeChannelRequestType = undefined

writeDescription :: Description -> Write
writeDescription = undefined

writeSSHString :: ByteString -> Write
writeSSHString bs = writeWord32BE (fromIntegral $ BS.length bs)
                 <> writeByteString bs

writeByteString :: ByteString -> Write
writeByteString = undefined

writeByte :: Word8 -> Write
writeByte = writeStorable

writeWord32BE :: Word32BE -> Write
writeWord32BE = write

writeNameList :: NameList -> Write
writeNameList = undefined

writeText :: Text -> Write
writeText = undefined

writeBool :: Bool -> Write
writeBool True  = writeByte 1
writeBool False = writeByte 0
