{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}

module Raaz.Cipher.AES.ECB.Type where

import           Data.ByteString           (ByteString)
import           Raaz.Primitives
import           Raaz.Primitives.Cipher
import           Raaz.Types

import           Raaz.Cipher.AES.Ref.Internal ()
import           Raaz.Cipher.AES.Internal


-- | Primitive for AES128 in ECB Mode
instance Primitive (AES128 ECB Encryption) where
  blockSize _ = cryptoCoerce $ BITS (128 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES128 ECB Encryption) = AES128EIV ByteString
  primitiveName _ = "AES128 ECB Encryption"

instance Primitive (AES128 ECB Decryption) where
  blockSize _ = cryptoCoerce $ BITS (128 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES128 ECB Decryption) = AES128DIV ByteString
  primitiveName _ = "AES128 ECB Decryption"

-- | Primitive for AES192 in ECB Mode
instance Primitive (AES192 ECB Encryption) where
  blockSize _ = cryptoCoerce $ BITS (128 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES192 ECB Encryption) = AES192EIV ByteString
  primitiveName _ = "AES192 ECB Encryption"

instance Primitive (AES192 ECB Decryption) where
  blockSize _ = cryptoCoerce $ BITS (128 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES192 ECB Decryption) = AES192DIV ByteString
  primitiveName _ = "AES192 ECB Decryption"

-- | Primitive for AES256 in ECB Mode
instance Primitive (AES256 ECB Encryption) where
  blockSize _ = cryptoCoerce $ BITS (128 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES256 ECB Encryption) = AES256EIV ByteString
  primitiveName _ = "AES256 ECB Encryption"

instance Primitive (AES256 ECB Decryption) where
  blockSize _ = cryptoCoerce $ BITS (128 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES256 ECB Decryption) = AES256DIV ByteString
  primitiveName _ = "AES256 ECB Decryption"

instance Initializable (AES128 ECB Encryption) where
  ivSize _ = BYTES 16
  getIV = AES128EIV

instance Initializable (AES128 ECB Decryption) where
  ivSize _ = BYTES 16
  getIV = AES128DIV

instance Initializable (AES192 ECB Encryption) where
  ivSize _ = BYTES 24
  getIV = AES192EIV

instance Initializable (AES192 ECB Decryption) where
  ivSize _ = BYTES 24
  getIV = AES192DIV

instance Initializable (AES256 ECB Encryption) where
  ivSize _ = BYTES 32
  getIV = AES256EIV

instance Initializable (AES256 ECB Decryption) where
  ivSize _ = BYTES 32
  getIV = AES256DIV
