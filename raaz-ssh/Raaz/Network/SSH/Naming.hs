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
instance MethodName (Cipher (AES CBC) KEY128 s) where
  methodName _ = "aes128-cbc"

-- | IANA standard name for AES192 CBC
instance MethodName (Cipher (AES CBC) KEY192 s) where
  methodName _ = "aes192-cbc"

-- | IANA standard name for AES256 CBC
instance MethodName (Cipher (AES CBC) KEY256 s) where
  methodName _ = "aes256-cbc"

-- | IANA standard name for AES128 CTR
instance MethodName (Cipher (AES CTR) KEY128 s) where
  methodName _ = "aes128-ctr"

-- | IANA standard name for AES192 CTR
instance MethodName (Cipher (AES CTR) KEY192 s) where
  methodName _ = "aes192-ctr"

-- | IANA standard name for AES256 CTR
instance MethodName (Cipher (AES CTR) KEY256 s) where
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

aes256ctrE :: Cipher (AES CTR) KEY256 Encryption
aes256ctrE = undefined

aes256cbcE :: Cipher (AES CBC) KEY256 Encryption
aes256cbcE = undefined

aes192ctrE :: Cipher (AES CTR) KEY192 Encryption
aes192ctrE = undefined

aes192cbcE :: Cipher (AES CBC) KEY192 Encryption
aes192cbcE = undefined

aes128ctrE :: Cipher (AES CTR) KEY128 Encryption
aes128ctrE = undefined

aes128cbcE :: Cipher (AES CBC) KEY128 Encryption
aes128cbcE = undefined

aes256ctrD :: Cipher (AES CTR) KEY256 Decryption
aes256ctrD = undefined

aes256cbcD :: Cipher (AES CBC) KEY256 Decryption
aes256cbcD = undefined

aes192ctrD :: Cipher (AES CTR) KEY192 Decryption
aes192ctrD = undefined

aes192cbcD :: Cipher (AES CBC) KEY192 Decryption
aes192cbcD = undefined

aes128ctrD :: Cipher (AES CTR) KEY128 Decryption
aes128ctrD = undefined

aes128cbcD :: Cipher (AES CBC) KEY128 Decryption
aes128cbcD = undefined
