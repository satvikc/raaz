{-|

Standard naming for primitives required while algorithm negotiation.

-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}
module Raaz.Network.SSH.Naming
       ( MethodName(..)
         -- | Hash Primitives (which are just `undefined` with the correct type)
       , sha1, sha224, sha256, sha384, sha512
         -- | AES Primitives (which are just `undefined` with the correct type)
       , aes256ctrE, aes192ctrE, aes128ctrE
       , aes256ctrD, aes192ctrD, aes128ctrD
       , aes256cbcE, aes192cbcE, aes128cbcE
       , aes256cbcD, aes192cbcD, aes128cbcD
       ) where


import           Raaz.Primitives
import           Raaz.Hash (SHA1,SHA224,SHA256,SHA384,SHA512)
import           Raaz.Primitives.Cipher
import           Raaz.Cipher.AES.Type
import           Raaz.Cipher.AES.CBC    ()
import           Raaz.Cipher.AES.CTR    ()

import qualified Data.ByteString.Char8  as C8

-- | Standard Naming required while algorithm negotiations.
class MethodName p where
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

aes256ctrE :: AES256 CTR Encryption
aes256ctrE = undefined

aes256cbcE :: AES256 CBC Encryption
aes256cbcE = undefined

aes192ctrE :: AES192 CTR Encryption
aes192ctrE = undefined

aes192cbcE :: AES192 CBC Encryption
aes192cbcE = undefined

aes128ctrE :: AES128 CTR Encryption
aes128ctrE = undefined

aes128cbcE :: AES128 CBC Encryption
aes128cbcE = undefined

aes256ctrD :: AES256 CTR Decryption
aes256ctrD = undefined

aes256cbcD :: AES256 CBC Decryption
aes256cbcD = undefined

aes192ctrD :: AES192 CTR Decryption
aes192ctrD = undefined

aes192cbcD :: AES192 CBC Decryption
aes192cbcD = undefined

aes128ctrD :: AES128 CTR Decryption
aes128ctrD = undefined

aes128cbcD :: AES128 CBC Decryption
aes128cbcD = undefined
