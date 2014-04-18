{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Raaz.Cipher.AES.ECB.Instance where

import Raaz.Core.Crypto
import Raaz.Core.Crypto.Cipher

import Raaz.Cipher.AES.ECB.Ref       ()
import Raaz.Cipher.AES.ECB.CPortable ()
import Raaz.Cipher.AES.Internal

instance CryptoPrimitive (Cipher (AES ECB) KEY128 EncryptMode) where
  type Recommended (Cipher (AES ECB) KEY128 EncryptMode) = CGadget (Cipher (AES ECB) KEY128 EncryptMode)
  type Reference (Cipher (AES ECB) KEY128 EncryptMode) = HGadget (Cipher (AES ECB) KEY128 EncryptMode)

instance CryptoPrimitive (Cipher (AES ECB) KEY128 DecryptMode) where
  type Recommended (Cipher (AES ECB) KEY128 DecryptMode) = CGadget (Cipher (AES ECB) KEY128 DecryptMode)
  type Reference (Cipher (AES ECB) KEY128 DecryptMode) = HGadget (Cipher (AES ECB) KEY128 DecryptMode)

instance CryptoPrimitive (Cipher (AES ECB) KEY192 EncryptMode) where
  type Recommended (Cipher (AES ECB) KEY192 EncryptMode) = CGadget (Cipher (AES ECB) KEY192 EncryptMode)
  type Reference (Cipher (AES ECB) KEY192 EncryptMode) = HGadget (Cipher (AES ECB) KEY192 EncryptMode)

instance CryptoPrimitive (Cipher (AES ECB) KEY192 DecryptMode) where
  type Recommended (Cipher (AES ECB) KEY192 DecryptMode) = CGadget (Cipher (AES ECB) KEY192 DecryptMode)
  type Reference (Cipher (AES ECB) KEY192 DecryptMode) = HGadget (Cipher (AES ECB) KEY192 DecryptMode)

instance CryptoPrimitive (Cipher (AES ECB) KEY256 EncryptMode) where
  type Recommended (Cipher (AES ECB) KEY256 EncryptMode) = CGadget (Cipher (AES ECB) KEY256 EncryptMode)
  type Reference (Cipher (AES ECB) KEY256 EncryptMode) = HGadget (Cipher (AES ECB) KEY256 EncryptMode)

instance CryptoPrimitive (Cipher (AES ECB) KEY256 DecryptMode) where
  type Recommended (Cipher (AES ECB) KEY256 DecryptMode) = CGadget (Cipher (AES ECB) KEY256 DecryptMode)
  type Reference (Cipher (AES ECB) KEY256 DecryptMode) = HGadget (Cipher (AES ECB) KEY256 DecryptMode)
