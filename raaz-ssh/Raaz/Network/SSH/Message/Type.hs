module Raaz.Network.SSH.Message.Type where

import Data.ByteString.Char8 (ByteString)

import Raaz.Types

-- | Types of messages
data Message = Trans    Transport
             | Auth     Authentication
             | Connect  Connection
             | Client   ClientProtocol
             | Ext      Extension

-- | Standard Transport Layer Messages
data Transport = Disconnect DisconnectReason Description
               | Ignore ByteString
                 -- | Unimplemented response with packet sequence number
               | Unimplemented Word32BE
                 -- | Debug message. Bool specifies whether to display
                 -- debug information to client or not.
               | Debug Bool Description
               | ServiceRequest Service
               | ServiceAccept Service
               | Kexinit { kexinit_cookie          :: ByteString  -- ^ Random cookie of 16 Bytes
                         , kexinit_keyexhange      :: NameList    -- ^ Key Exchange Algorithms
                         , kexinit_hostkey         :: NameList    -- ^ Server Host Key Algorithms
                         , kexinit_encryption_c2s  :: NameList    -- ^ Encryption Algorithms for Client to Server
                         , kexinit_encryption_s2c  :: NameList    -- ^ Encryption Algorithms for Server to Client
                         , kexinit_mac_c2s         :: NameList    -- ^ MAC algorithms for Client to Server
                         , kexinit_mac_s2c         :: NameList    -- ^ MAC algorithms for Server to Client
                         , kexinit_compression_c2s :: NameList    -- ^ Compression algorithms for Client to Server
                         , kexinit_compression_s2c :: NameList    -- ^ Compression algorithms for Server to Client
                         , kexinit_language_c2s    :: NameList    -- ^ Languages algorithms for Client to Server
                         , kexinit_language_s2c    :: NameList    -- ^ Languages algorithms for Server to Client
                         , kexinit_guessed         :: Bool        -- ^ Guessed Key Exchange Packet follows
                         }
                 -- | Sent when key exchange is successful
               | Newkeys

-- | Standard Authentication Layer Messages
data Authentication = Request { auth_username :: Text
                              , auth_service  :: Service
                              , auth_method   :: AuthMethod
                              }
                      -- | Authentications that can continue. Bool specifies partial success.
                    | Failure NameList Bool
                      -- | Authentication Success
                    | Success
                      -- | Banner text to be displayed to the Client before authentication
                    | Banner Description

-- | Standard Connection Layer Messages
data Connection =
    -- | Global request. Bool specifies if reply needed or not.
    GlobalRequest GlobalRequestType Bool
    -- | Global request success
  | RequestSuccess GlobalRequestSuccessData
    -- | Global request failure
  | RequestFailure
  | ChannelOpen { open_type   :: ChannelType
                , open_sender :: ChannelID
                , open_window :: Word32BE
                , open_packet :: Word32BE
                }
  | ChannelOpenConfirmation { conf_recipient :: ChannelID
                            , conf_sender    :: ChannelID
                            , conf_window    :: Word32BE
                            , conf_packet    :: Word32BE
                            , conf_data      :: ConfirmationData
                            }
  | ChannelOpenFailure ChannelID ChannelOpenFailureReason Description
    -- | Channel Id and Bytes to add
  | ChannelWindowAdjust ChannelID Word32BE
    -- | Channel Id and data to send
  | ChannelData ChannelID ByteString
    -- | Extended data over a channel
  | ChannelExtendedData ChannelID ExtendedDataType ByteString
    -- | Channel EOF
  | ChannelEOF ChannelID
    -- | Channel Close
  | ChannelClose ChannelID
    -- | Channel request with id and type. Bool specifies if reply is needed or not.
  | ChannelRequest ChannelID ChannelRequestType Bool
    -- | Request success
  | ChannelSuccess ChannelID
    -- | Request Failure
  | ChannelFailure ChannelID

-- | Client Protocol Messages (/Unimplemented/)
data ClientProtocol = ClientProtocol

-- | Message Extensions (/Unimplemented/)
data Extension = Extension

-- | Services
data Service = Userauth
             | Connection


-- | User Authentication Methods
data AuthMethod = Publickey { pubkey_name      :: ByteString
                            , pubkey_blob      :: ByteString
                            , pubkey_signature :: (Maybe ByteString)
                            }
                | Password { passwd_plain :: Text         -- ^ Plain text password (or old password)
                           , passwd_new   :: (Maybe Text) -- ^ New password in case of password change request
                           }
                | Hostbased -- ^ /Not implemented/
                | None      -- ^ No Authentication

-- | Data to be signed for public key authentication
data SignatureObject = SignatureObject { sig_identifier :: ByteString
                                       , sig_username   :: ByteString
                                       , sig_service    :: ByteString
                                       , sig_pubkeyalgo :: ByteString
                                       , sig_pubkey     :: ByteString
                                       }

-- | Public Key Authentication specific messages
data PKMessage = PKOK ByteString ByteString -- ^ Algorithm name and public key blob

-- | Password Authentication specific messages
data PassMessage = PasswdChangereq Description -- ^ Prompt

-- | Global requests
data GlobalRequestType = TCPIPForward ByteString Word32BE       -- ^ Address and port
                       | CancelTCPIPForward ByteString Word32BE -- ^ Address and port

-- | Global Request Succest Data
data GlobalRequestSuccessData = NoSuccessData -- ^ No data
                              | Word32BE      -- ^ TCP port allocated at server

-- | Channel Types
data ChannelType = Session
                   -- | Originator address and port
                 | X11 ByteString Word32BE
                 | ForwardedTCPIP { ftcpid_addr     :: ByteString -- ^ Address that was connected
                                  , ftcpip_port     :: Word32BE   -- ^ Port that was connected
                                  , ftcpip_origaddr :: ByteString -- ^ Originator IP address
                                  , ftcpip_origport :: Word32BE   -- ^ Originator Port
                                  }
                 | DirectTCPIP { dtcpid_addr     :: ByteString -- ^ Host to connect
                               , dtcpip_port     :: Word32BE   -- ^ Port to connect
                               , dtcpip_origaddr :: ByteString -- ^ Originator IP address
                               , dtcpip_origport :: Word32BE   -- ^ Originator Port
                               }

-- | Confirmation Data (I can not find any channel specific
-- confirmation data but it is mentioned in the standard that there
-- can be such data)
data ConfirmationData = NoConfirmationData

-- | Channel Request Types
data ChannelRequestType = PtyReq { ptyreq_env     :: ByteString -- ^ Terminal environment variable
                                 , ptyreq_width   :: Word32BE
                                 , ptyreq_height  :: Word32BE
                                 , ptyreq_pwidth  :: Word32BE
                                 , ptyreq_pheight :: Word32BE
                                 , ptyreq_mode    :: ByteString -- ^ Encoded Terminal modes
                                 }
                        | X11Req { x11req_single   :: Bool       -- ^ Single Connection
                                 , x11req_protocol :: ByteString -- ^ X11 Authentication Protocol
                                 , x11req_cookie   :: ByteString -- ^ X11 Authentication Cookie
                                 , x11req_screen   :: Word32BE   -- ^ X11 Screen Number
                                 }
                          -- | Environment variable name and value
                        | Env ByteString ByteString
                        | Shell
                        | Exec ByteString
                        | Subsystem ByteString
                        | WindowChange { win_width   :: Word32BE
                                       , win_height  :: Word32BE
                                       , win_pwidth  :: Word32BE
                                       , win_pheight :: Word32BE
                                       }
                          -- | Client can do flow control
                        | XonXoff Bool
                        | Signal SignalName
                        | ExitStatus Word32BE
                          -- | Bool specifies if code was dumped
                        | ExitSignal SignalName Bool Description

-- | Disconnection Reasons
data DisconnectReason = HostNotAllowedToConnect
                      | ProtocolError
                      | KeyExchangeFailed
                      | Reserved
                      | MacError
                      | CompressionError
                      | ServiceNotAvailable
                      | ProtocolVersionNotSupported
                      | HostKeyNotVerifiable
                      | ConnectionLost
                      | ByApplication
                      | TooManyConnections
                      | AuthCancelledByUser
                      | NoMoreAuthMethodsAvailable
                      | IllegalUserName

-- | Channel Open Failure Reasons
data ChannelOpenFailureReason = AdministrativelyProhibited
                              | ConnectFailed
                              | UnknownChannelType
                              | ResourceShortage


-- | Channel Extended Data Types
data ExtendedDataType = Stderr

-- | Signal Names
data SignalName = ABRT
                | ALRM
                | FPE
                | HUP
                | ILL
                | INT
                | KILL
                | PIPE
                | QUIT
                | SEGV
                | TERM
                | USR1
                | USR2

-- | Encoded Description in ISO-10646 UTF-8 encoding (TODO) along with Language tag
data Description = Description Text ByteString

data Text = Text

type NameList = [ByteString]

type ChannelID = Word32BE
