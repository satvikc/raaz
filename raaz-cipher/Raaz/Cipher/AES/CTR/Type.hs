{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}

module Raaz.Cipher.AES.CTR.Type where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Foreign.Storable
import           Raaz.Primitives
import           Raaz.Primitives.Cipher
import           Raaz.Types

import           Raaz.Cipher.AES.Ref.Type
import           Raaz.Cipher.AES.Ref.Internal ()
import           Raaz.Cipher.AES.Internal


-- | Primitive for AES128 in CTR Mode
instance Primitive (AES128 CTR Encryption) where
  blockSize _ = cryptoCoerce $ BITS (8 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES128 CTR Encryption) = AES128EIV (ByteString, ByteString)
  primitiveName _ = "AES128 CTR Encryption"

instance Primitive (AES128 CTR Decryption) where
  blockSize _ = cryptoCoerce $ BITS (8 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES128 CTR Decryption) = AES128DIV (ByteString, ByteString)
  primitiveName _ = "AES128 CTR Decryption"

-- | Primitive for AES192 in CTR Mode
instance Primitive (AES192 CTR Encryption) where
  blockSize _ = cryptoCoerce $ BITS (8 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES192 CTR Encryption) = AES192EIV (ByteString, ByteString)
  primitiveName _ = "AES192 CTR Encryption"

instance Primitive (AES192 CTR Decryption) where
  blockSize _ = cryptoCoerce $ BITS (8 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES192 CTR Decryption) = AES192DIV (ByteString, ByteString)
  primitiveName _ = "AES192 CTR Decryption"

-- | Primitive for AES256 in CTR Mode
instance Primitive (AES256 CTR Encryption) where
  blockSize _ = cryptoCoerce $ BITS (8 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES256 CTR Encryption) = AES256EIV (ByteString, ByteString)
  primitiveName _ = "AES256 CTR Encryption"

instance Primitive (AES256 CTR Decryption) where
  blockSize _ = cryptoCoerce $ BITS (8 :: Int)
  {-# INLINE blockSize #-}
  newtype IV (AES256 CTR Decryption) = AES256DIV (ByteString, ByteString)
  primitiveName _ = "AES256 CTR Decryption"

-- | First KEY then 128bit initialization vector
getIVCTR :: (Storable k) => ByteString -> k -> (ByteString,ByteString)
getIVCTR bs k = BS.splitAt (sizeOf k) bs

instance Initializable (AES128 CTR Encryption) where
  ivSize _ = BYTES (16 + 16)
  getIV src = AES128EIV $ getIVCTR src (undefined :: KEY128)

instance Initializable (AES128 CTR Decryption) where
  ivSize _ = BYTES (16 + 16)
  getIV src = AES128DIV $ getIVCTR src (undefined :: KEY128)

instance Initializable (AES192 CTR Encryption) where
  ivSize _ = BYTES (24 + 16)
  getIV src = AES192EIV $ getIVCTR src (undefined :: KEY192)

instance Initializable (AES192 CTR Decryption) where
  ivSize _ = BYTES (24 + 16)
  getIV src = AES192DIV $ getIVCTR src (undefined :: KEY192)

instance Initializable (AES256 CTR Encryption) where
  ivSize _ = BYTES (32 + 16)
  getIV src = AES256EIV $ getIVCTR src (undefined :: KEY256)

instance Initializable (AES256 CTR Decryption) where
  ivSize _ = BYTES (32 + 16)
  getIV src = AES256DIV $ getIVCTR src (undefined :: KEY256)
