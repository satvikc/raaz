{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raaz.Network.SSH.Message.Parser where

import           Control.Applicative
import           Control.Exception
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as BC
import           Data.Text
import           Data.Text.Encoding
import           Data.Typeable
import           Data.Word


import           Raaz.Parse                    (Parser,parse,parseStorable,parseByteString)
import           Raaz.Types

import           Raaz.Network.SSH.Message.Type

data ParseError = ParseError String deriving (Show,Typeable)

instance Exception ParseError

parseMessage :: Parser Message
parseMessage = decideParse =<< parseByte

decideParse :: Word8 -> Parser Message
decideParse 1  = Disconnect <$> (toEnum . fromIntegral <$> parseWord32BE)
                            <*> parseDescription
decideParse 2  = Ignore <$> parseSSHString
decideParse 3  = Unimplemented <$> parse
decideParse 4  = Debug <$> parseBool <*> parseDescription
decideParse 5  = ServiceRequest <$> parseService
decideParse 6  = ServiceAccept <$> parseService
decideParse 20 = Kexinit <$> parseByteString (16 :: BYTES Int)
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseNameList
                         <*> parseBool
decideParse 21 = pure Newkeys
decideParse 50 = Request <$> parseUTF8 <*> parseService <*> parseAuthMethod
decideParse 51 = Failure <$> parseNameList <*> parseBool
decideParse 52 = pure Success
decideParse 53 = Banner <$> parseDescription
decideParse 80 = parseSSHString >>= (GlobalRequest <$> parseBool <*>) . parseRequest
  where
    parseRequest "tcpip-forward" = TCPIPForward <$> parseSSHString <*> parse
    parseRequest "cancel-tcpip-forward" = CancelTCPIPForward <$> parseSSHString
                                                             <*> parse
    parseRequest _ = throw $ ParseError "Unknown Global Request"
decideParse 81 = RequestSuccess <$> undefined -- parse rest of data
decideParse 82 = pure RequestFailure
decideParse 90 = parseSSHString
             >>= (ChannelOpen <$> parse <*> parse <*> parse <*>) . parseType
  where
    parseType "session" = pure Session
    parseType "x11" = X11 <$> parseSSHString <*> parse
    parseType "forwarded-tcpip" = ForwardedTCPIP <$> parseSSHString
                                                 <*> parse
                                                 <*> parseSSHString
                                                 <*> parse
    parseType "direct-tcpip" = DirectTCPIP <$> parseSSHString
                                           <*> parse
                                           <*> parseSSHString
                                           <*> parse
decideParse 91 = ChannelOpenConfirmation <$> parse
                                         <*> parse
                                         <*> parse
                                         <*> parse
                                         <*> undefined -- parse rest of data
decideParse 92 = ChannelOpenFailure <$> parse
                 <*> (toEnum . fromIntegral <$> parseWord32BE)
                                    <*> parseDescription
decideParse 93 = ChannelWindowAdjust <$> parse <*> parse
decideParse 94 = ChannelData <$> parse <*> parseSSHString
decideParse 95 = ChannelExtendedData <$> parse <*> parseExtendedDataType <*> parseSSHString
decideParse 96 = ChannelEOF <$> parse
decideParse 97 = ChannelClose <$> parse
decideParse 98 = ChannelRequest <$> parse <*> parseChannelRequestType <*> parseBool
decideParse 99 = ChannelSuccess <$> parse
decideParse 100 = ChannelFailure <$> parse
decideParse _ = throw $ ParseError "Unkown Transport Message"

parseService :: Parser Service
parseService = check <$> parseSSHString
  where
    check "ssh-userauth"   = Userauth
    check "ssh-connection" = Connection
    check _ = throw $ ParseError "Unknown Service"

parseAuthMethod :: Parser AuthMethod
parseAuthMethod = check =<< parseSSHString
  where
    check "publickey" = checkSig =<< parseBool
      where
        checkSig False = PublicKey <$> parseSSHString
                                   <*> parseSSHString
                                   <*> pure Nothing
        checkSig True  = PublicKey <$> parseSSHString
                                   <*> parseSSHString
                                   <*> (Just <$> parseSSHString)
    check "password" = checkChange =<< parseBool
      where
        checkChange False = Password <$> parseUTF8 <*> pure Nothing
        checkChange True = Password <$> parseUTF8 <*> (Just <$> parseUTF8)
    check "hostbased" = undefined -- Not Implemented
    check "none" = pure None

parseExtendedDataType :: Parser ExtendedDataType
parseExtendedDataType = undefined

parseChannelRequestType :: Parser ChannelRequestType
parseChannelRequestType = undefined

parseSSHString :: Parser ByteString
parseSSHString = parseByteString . bytes =<< parseWord32BE

parseUTF8 :: Parser Text
parseUTF8 = decodeUtf8 <$> parseSSHString

parseDescription :: Parser Description
parseDescription = Description <$> parseUTF8 <*> parseSSHString

parseNameList :: Parser NameList
parseNameList = BC.split ',' <$> parseSSHString

parseBool :: Parser Bool
parseBool = check <$> parseStorable
  where
    check :: Word8 -> Bool
    check 0 = False
    check _ = True

parseWord32BE :: Parser Word32BE
parseWord32BE = parse

parseByte :: Parser Word8
parseByte = parseStorable

bytes :: Integral a => a -> BYTES Int
bytes = BYTES . fromIntegral
