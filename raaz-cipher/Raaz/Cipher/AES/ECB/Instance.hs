{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}

module Raaz.Cipher.AES.ECB.Instance where

import Raaz.Primitives
import Raaz.Primitives.Cipher

import Raaz.Cipher.AES.ECB.Ref       ()
import Raaz.Cipher.AES.ECB.CPortable ()
import Raaz.Cipher.AES.Internal

instance CryptoPrimitive (AES128 ECB Encryption) where
  type Recommended (AES128 ECB Encryption) = CPortable128 ECB Encryption
  type Reference (AES128 ECB Encryption) = Ref128 ECB Encryption

instance CryptoPrimitive (AES128 ECB Decryption) where
  type Recommended (AES128 ECB Decryption) = CPortable128 ECB Decryption
  type Reference (AES128 ECB Decryption) = Ref128 ECB Decryption

instance CryptoPrimitive (AES192 ECB Encryption) where
  type Recommended (AES192 ECB Encryption) = CPortable192 ECB Encryption
  type Reference (AES192 ECB Encryption) = Ref192 ECB Encryption

instance CryptoPrimitive (AES192 ECB Decryption) where
  type Recommended (AES192 ECB Decryption) = CPortable192 ECB Decryption
  type Reference (AES192 ECB Decryption) = Ref192 ECB Decryption

instance CryptoPrimitive (AES256 ECB Encryption) where
  type Recommended (AES256 ECB Encryption) = CPortable256 ECB Encryption
  type Reference (AES256 ECB Encryption) = Ref256 ECB Encryption

instance CryptoPrimitive (AES256 ECB Decryption) where
  type Recommended (AES256 ECB Decryption) = CPortable256 ECB Decryption
  type Reference (AES256 ECB Decryption) = Ref256 ECB Decryption

instance HasInverse (Ref128 ECB Encryption) where
  type Inverse (Ref128 ECB Encryption) = Ref128 ECB Decryption

instance HasInverse (Ref128 ECB Decryption) where
  type Inverse (Ref128 ECB Decryption) = Ref128 ECB Encryption

instance HasInverse (Ref192 ECB Encryption) where
  type Inverse (Ref192 ECB Encryption) = Ref192 ECB Decryption

instance HasInverse (Ref192 ECB Decryption) where
  type Inverse (Ref192 ECB Decryption) = Ref192 ECB Encryption

instance HasInverse (Ref256 ECB Encryption) where
  type Inverse (Ref256 ECB Encryption) = Ref256 ECB Decryption

instance HasInverse (Ref256 ECB Decryption) where
  type Inverse (Ref256 ECB Decryption) = Ref256 ECB Encryption

instance HasInverse (CPortable128 ECB Encryption) where
  type Inverse (CPortable128 ECB Encryption) = CPortable128 ECB Decryption

instance HasInverse (CPortable128 ECB Decryption) where
  type Inverse (CPortable128 ECB Decryption) = CPortable128 ECB Encryption

instance HasInverse (CPortable192 ECB Encryption) where
  type Inverse (CPortable192 ECB Encryption) = CPortable192 ECB Decryption

instance HasInverse (CPortable192 ECB Decryption) where
  type Inverse (CPortable192 ECB Decryption) = CPortable192 ECB Encryption

instance HasInverse (CPortable256 ECB Encryption) where
  type Inverse (CPortable256 ECB Encryption) = CPortable256 ECB Decryption

instance HasInverse (CPortable256 ECB Decryption) where
  type Inverse (CPortable256 ECB Decryption) = CPortable256 ECB Encryption
