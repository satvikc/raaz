{-|

Standard naming for primitives required while algorithm negotiation.

-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}
module Raaz.Network.SSH.Naming
       ( MethodName(..)
       ) where


import           Raaz.Primitives
import           Raaz.Hash
import           Raaz.Primitives.Cipher
import           Raaz.Cipher.AES.Type
import           Raaz.Cipher.AES.CBC    ()
import           Raaz.Cipher.AES.CTR    ()

import qualified Data.ByteString.Char8  as C8

-- | Standard Naming required while algorithm negotiations.
class Primitive p => MethodName p where
  methodName :: p -> C8.ByteString

-- | `MethodName` instances for hash algorithms.

-- | IANA standard name for `SHA1`
instance MethodName SHA1 where
  methodName _ = "sha-1"

-- | IANA standard name for `SHA224`
instance MethodName SHA224 where
  methodName _ = "sha-224"

-- | IANA standard name for `SHA256`
instance MethodName SHA256 where
  methodName _ = "sha-256"

-- | IANA standard name for `SHA384`
instance MethodName SHA384 where
  methodName _ = "sha-384"

-- | IANA standard name for `SHA512`
instance MethodName SHA512 where
  methodName _ = "sha-512"

-- | IANA standard name for AES128 CBC
instance MethodName (AES128 CBC Encryption) where
  methodName _ = "aes128-cbc"

instance MethodName (AES128 CBC Decryption) where
  methodName _ = "aes128-cbc"

-- | IANA standard name for AES192 CBC
instance MethodName (AES192 CBC Encryption) where
  methodName _ = "aes192-cbc"

instance MethodName (AES192 CBC Decryption) where
  methodName _ = "aes192-cbc"

-- | IANA standard name for AES256 CBC
instance MethodName (AES256 CBC Encryption) where
  methodName _ = "aes256-cbc"

instance MethodName (AES256 CBC Decryption) where
  methodName _ = "aes256-cbc"

-- | IANA standard name for AES128 CTR
instance MethodName (AES128 CTR Encryption) where
  methodName _ = "aes128-ctr"

instance MethodName (AES128 CTR Decryption) where
  methodName _ = "aes128-ctr"

-- | IANA standard name for AES192 CTR
instance MethodName (AES192 CTR Encryption) where
  methodName _ = "aes192-ctr"

instance MethodName (AES192 CTR Decryption) where
  methodName _ = "aes192-ctr"

-- | IANA standard name for AES256 CTR
instance MethodName (AES256 CTR Encryption) where
  methodName _ = "aes256-ctr"

instance MethodName (AES256 CTR Decryption) where
  methodName _ = "aes256-ctr"
