module Raaz.Network.SSH.Message.Type where

import Data.ByteString.Char8 (ByteString)
import Data.Text

import Raaz.Core.Types

-- | Message Id's
data MessageID = DISCONNECT
               | IGNORE
               | UNIMPLEMENTED
               | DEBUG
               | SERVICEREQUEST
               | SERVICEACCEPT
               | KEXINIT
               | NEWKEYS
               | REQUEST
               | FAILURE
               | SUCCESS
               | BANNER
               | GLOBALREQUEST
               | REQUESTSUCCESS
               | REQUESTFAILURE
               | CHANNELOPEN
               | CHANNELOPENCONFIRMATION
               | CHANNELOPENFAILURE
               | CHANNELWINDOWADJUST
               | CHANNELDATA
               | CHANNELEXTENDEDDATA
               | CHANNELEOF
               | CHANNELCLOSE
               | CHANNELREQUEST
               | CHANNELSUCCESS
               | CHANNELFAILURE

instance Bounded MessageID where
  minBound = DISCONNECT
  maxBound = CHANNELFAILURE

instance Enum MessageID where
  toEnum 1   = DISCONNECT
  toEnum 2   = IGNORE
  toEnum 3   = UNIMPLEMENTED
  toEnum 4   = DEBUG
  toEnum 5   = SERVICEREQUEST
  toEnum 6   = SERVICEACCEPT
  toEnum 20  = KEXINIT
  toEnum 21  = NEWKEYS
  toEnum 50  = REQUEST
  toEnum 51  = FAILURE
  toEnum 52  = SUCCESS
  toEnum 53  = BANNER
  toEnum 80  = GLOBALREQUEST
  toEnum 81  = REQUESTSUCCESS
  toEnum 82  = REQUESTFAILURE
  toEnum 90  = CHANNELOPEN
  toEnum 91  = CHANNELOPENCONFIRMATION
  toEnum 92  = CHANNELOPENFAILURE
  toEnum 93  = CHANNELWINDOWADJUST
  toEnum 94  = CHANNELDATA
  toEnum 95  = CHANNELEXTENDEDDATA
  toEnum 96  = CHANNELEOF
  toEnum 97  = CHANNELCLOSE
  toEnum 98  = CHANNELREQUEST
  toEnum 99  = CHANNELSUCCESS
  toEnum 100 = CHANNELFAILURE
  toEnum _   = error "Unknown Channel Message Id"
  fromEnum DISCONNECT              = 1
  fromEnum IGNORE                  = 2
  fromEnum UNIMPLEMENTED           = 3
  fromEnum DEBUG                   = 4
  fromEnum SERVICEREQUEST          = 5
  fromEnum SERVICEACCEPT           = 6
  fromEnum KEXINIT                 = 20
  fromEnum NEWKEYS                 = 21
  fromEnum REQUEST                 = 50
  fromEnum FAILURE                 = 51
  fromEnum SUCCESS                 = 52
  fromEnum BANNER                  = 53
  fromEnum GLOBALREQUEST           = 80
  fromEnum REQUESTSUCCESS          = 81
  fromEnum REQUESTFAILURE          = 82
  fromEnum CHANNELOPEN             = 90
  fromEnum CHANNELOPENCONFIRMATION = 91
  fromEnum CHANNELOPENFAILURE      = 92
  fromEnum CHANNELWINDOWADJUST     = 93
  fromEnum CHANNELDATA             = 94
  fromEnum CHANNELEXTENDEDDATA     = 95
  fromEnum CHANNELEOF              = 96
  fromEnum CHANNELCLOSE            = 97
  fromEnum CHANNELREQUEST          = 98
  fromEnum CHANNELSUCCESS          = 99
  fromEnum CHANNELFAILURE          = 100

-- | Types of messages
data Message =
              -- | Transport Layer
               Disconnect DisconnectReason Description
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

               -- Authentication Layer

             | Request { auth_username :: Text
                       , auth_service  :: Service
                       , auth_method   :: AuthMethod
                       }
               -- | Authentications that can continue. Bool specifies partial success.
             | Failure NameList Bool
               -- | Authentication Success
             | Success
               -- | Banner text to be displayed to the Client before authentication
             | Banner Description

               -- Connection Layer

               -- | Global request. Bool specifies if reply needed or not.
             | GlobalRequest Bool GlobalRequestType
               -- | Global request success. ByteString contains
               -- success specific data.
             | RequestSuccess ByteString
               -- | Global request failure
             | RequestFailure
             | ChannelOpen { open_sender :: ChannelID
                           , open_window :: Word32BE
                           , open_packet :: Word32BE
                           , open_type   :: ChannelType
                           }
             | ChannelOpenConfirmation { conf_recipient :: ChannelID
                                       , conf_sender    :: ChannelID
                                       , conf_window    :: Word32BE
                                       , conf_packet    :: Word32BE
                                       , conf_data      :: ByteString
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

-- | Services
data Service = Userauth
             | Connection

-- | User Authentication Methods
data AuthMethod = PublicKey { pubkey_name      :: ByteString
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
data GlobalRequestSuccessData = NoSuccessData         -- ^ No data
                             | SuccessPort Word32BE  -- ^ TCP port allocated at server

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

instance Bounded DisconnectReason where
  minBound = HostNotAllowedToConnect
  maxBound = IllegalUserName

instance Enum DisconnectReason where
  toEnum 1  = HostNotAllowedToConnect
  toEnum 2  = ProtocolError
  toEnum 3  = KeyExchangeFailed
  toEnum 4  = Reserved
  toEnum 5  = MacError
  toEnum 6  = CompressionError
  toEnum 7  = ServiceNotAvailable
  toEnum 8  = ProtocolVersionNotSupported
  toEnum 9  = HostKeyNotVerifiable
  toEnum 10 = ConnectionLost
  toEnum 11 = ByApplication
  toEnum 12 = TooManyConnections
  toEnum 13 = AuthCancelledByUser
  toEnum 14 = NoMoreAuthMethodsAvailable
  toEnum 15 = IllegalUserName
  toEnum _  = error "Unknown Disconnect Reason"
  fromEnum HostNotAllowedToConnect = 1
  fromEnum ProtocolError = 2
  fromEnum KeyExchangeFailed = 3
  fromEnum Reserved = 4
  fromEnum MacError = 5
  fromEnum CompressionError = 6
  fromEnum ServiceNotAvailable = 7
  fromEnum ProtocolVersionNotSupported = 8
  fromEnum HostKeyNotVerifiable = 9
  fromEnum ConnectionLost = 10
  fromEnum ByApplication = 11
  fromEnum TooManyConnections = 12
  fromEnum AuthCancelledByUser = 13
  fromEnum NoMoreAuthMethodsAvailable = 14
  fromEnum IllegalUserName = 15


-- | Channel Open Failure Reasons
data ChannelOpenFailureReason = AdministrativelyProhibited
                              | ConnectFailed
                              | UnknownChannelType
                              | ResourceShortage


instance Bounded ChannelOpenFailureReason where
  minBound = AdministrativelyProhibited
  maxBound = ResourceShortage

instance Enum ChannelOpenFailureReason where
  toEnum 1  = AdministrativelyProhibited
  toEnum 2  = ConnectFailed
  toEnum 3  = UnknownChannelType
  toEnum 4  = ResourceShortage
  toEnum _  = error "Unknown Channel Failure Reason"
  fromEnum AdministrativelyProhibited = 1
  fromEnum ConnectFailed              = 2
  fromEnum UnknownChannelType         = 3
  fromEnum ResourceShortage           = 4

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

type NameList = [ByteString]

type ChannelID = Word32BE
