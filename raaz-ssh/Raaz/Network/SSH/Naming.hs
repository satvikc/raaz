{-|

Standard naming for primitives required while algorithm negotiation.

-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DataKinds          #-}
module Raaz.Network.SSH.Naming
       ( MethodName(..)
         -- | Hash Primitives (which are just `undefined` with the correct type)
       , sha1, sha224, sha256, sha384, sha512
         -- | AES Primitives (which are just `undefined` with the correct type)
       , aes256ctr, aes192ctr, aes128ctr
       , aes256cbc, aes192cbc, aes128cbc
       , rsa1024sign, rsa2048sign
       , rsa1024verify, rsa2048verify
       ) where


import           Raaz.Core.Primitives
import           Raaz.Hash                   (SHA1,SHA224,SHA256,SHA384,SHA512)
import           Raaz.Core.Primitives.Cipher
import           Raaz.Core.Primitives.HMAC
import           Raaz.Cipher.AES
import           Raaz.Number
import           Raaz.RSA.Signature
import           Raaz.DH

import qualified Data.ByteString.Char8       as C8

-- | Standard Naming required while algorithm negotiations.
class MethodName p where
  methodName :: p -> C8.ByteString

-- | `MethodName` instances for hash algorithms.

-- | IANA standard name for `SHA1`
instance MethodName SHA1 where
  methodName _ = "sha-1"

instance MethodName (HMAC SHA1) where
  methodName _ = "hmac-sha1"

-- | IANA standard name for `SHA224`
instance MethodName SHA224 where
  methodName _ = "sha-224"

instance MethodName (HMAC SHA224) where
  methodName _ = "hmac-sha224"

-- | IANA standard name for `SHA256`
instance MethodName SHA256 where
  methodName _ = "sha-256"

instance MethodName (HMAC SHA256) where
  methodName _ = "hmac-sha256"

-- | IANA standard name for `SHA384`
instance MethodName SHA384 where
  methodName _ = "sha-384"

instance MethodName (HMAC SHA384) where
  methodName _ = "hmac-sha384"

-- | IANA standard name for `SHA512`
instance MethodName SHA512 where
  methodName _ = "sha-512"

instance MethodName (HMAC SHA512) where
  methodName _ = "hmac-sha512"

-- | IANA standard name for AES128 CBC
instance MethodName (AES CBC KEY128) where
  methodName _ = "aes128-cbc"

-- | IANA standard name for AES192 CBC
instance MethodName (AES CBC KEY192) where
  methodName _ = "aes192-cbc"

-- | IANA standard name for AES256 CBC
instance MethodName (AES CBC KEY256) where
  methodName _ = "aes256-cbc"

-- | IANA standard name for AES128 CTR
instance MethodName (AES CTR KEY128) where
  methodName _ = "aes128-ctr"

-- | IANA standard name for AES192 CTR
instance MethodName (AES CTR KEY192) where
  methodName _ = "aes192-ctr"

-- | IANA standard name for AES256 CTR
instance MethodName (AES CTR KEY256) where
  methodName _ = "aes256-ctr"

instance MethodName DHOakley1 where
  methodName _ = "diffie-hellman-group1"

instance MethodName DHOakley14 where
  methodName _ = "diffie-hellman-group14"

instance MethodName (RSA Word1024 SHA1 PKCS SignMode) where
  methodName _ = "ssh-rsa"


sha1 :: SHA1
sha1 = undefined

sha224 :: SHA224
sha224 = undefined

sha256 :: SHA256
sha256 = undefined

sha384 :: SHA384
sha384 = undefined

sha512 :: SHA512
sha512 = undefined

aes256ctr :: AES CTR KEY256
aes256ctr = undefined

aes256cbc :: AES CBC KEY256
aes256cbc = undefined

aes192ctr :: AES CTR KEY192
aes192ctr = undefined

aes192cbc :: AES CBC KEY192
aes192cbc = undefined

aes128ctr :: AES CTR KEY128
aes128ctr = undefined

aes128cbc :: AES CBC KEY128
aes128cbc = undefined

rsa1024sign :: RSA Word1024 SHA1 PKCS SignMode
rsa1024sign = undefined

rsa2048sign :: RSA Word2048 SHA1 PKCS SignMode
rsa2048sign = undefined

rsa1024verify :: RSA Word1024 SHA1 PKCS VerifyMode
rsa1024verify = undefined

rsa2048verify :: RSA Word2048 SHA1 PKCS VerifyMode
rsa2048verify = undefined
