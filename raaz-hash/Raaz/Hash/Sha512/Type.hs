{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Raaz.Hash.Sha512.Type
       ( SHA512(..)
       , Cxt(SHA512Cxt)
       ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Bits           ( xor, (.|.)   )
import Data.Default
import Data.Monoid
import Data.Word
import Data.Typeable       ( Typeable     )
import Foreign.Ptr         ( castPtr      )
import Foreign.Storable    ( Storable(..) )

import Raaz.Core.Parse.Unsafe
import Raaz.Core.Primitives
import Raaz.Core.Types
import Raaz.Core.Write.Unsafe

import Raaz.Hash.Sha.Util

----------------------------- SHA512 -------------------------------------------

-- | The Sha512 hash value. Used in implementation of Sha384 as well.
data SHA512 = SHA512 {-# UNPACK #-} !(BE Word64)
                     {-# UNPACK #-} !(BE Word64)
                     {-# UNPACK #-} !(BE Word64)
                     {-# UNPACK #-} !(BE Word64)
                     {-# UNPACK #-} !(BE Word64)
                     {-# UNPACK #-} !(BE Word64)
                     {-# UNPACK #-} !(BE Word64)
                     {-# UNPACK #-} !(BE Word64) deriving (Show, Typeable)

-- | Timing independent equality testing for sha512
instance Eq SHA512 where
  (==) (SHA512 g0 g1 g2 g3 g4 g5 g6 g7) (SHA512 h0 h1 h2 h3 h4 h5 h6 h7)
      =   xor g0 h0
      .|. xor g1 h1
      .|. xor g2 h2
      .|. xor g3 h3
      .|. xor g4 h4
      .|. xor g5 h5
      .|. xor g6 h6
      .|. xor g7 h7
      == 0

instance HasName SHA512

instance Digestible SHA512 where
  type Digest SHA512 = SHA512
  toDigest (SHA512Cxt h) = h

instance Storable SHA512 where
  sizeOf    _ = 8 * sizeOf (undefined :: (BE Word64))
  alignment _ = alignment  (undefined :: (BE Word64))

  peek ptr = runParser cptr parseSHA512
    where parseSHA512 = SHA512 <$> parseStorable
                               <*> parseStorable
                               <*> parseStorable
                               <*> parseStorable
                               <*> parseStorable
                               <*> parseStorable
                               <*> parseStorable
                               <*> parseStorable
          cptr = castPtr ptr

  poke ptr (SHA512 h0 h1 h2 h3 h4 h5 h6 h7) =  runWrite cptr writeSHA512
    where writeSHA512 =  writeStorable h0
                      <> writeStorable h1
                      <> writeStorable h2
                      <> writeStorable h3
                      <> writeStorable h4
                      <> writeStorable h5
                      <> writeStorable h6
                      <> writeStorable h7
          cptr = castPtr ptr

instance EndianStore SHA512 where
  load cptr = runParser cptr parseSHA512
    where parseSHA512 = SHA512 <$> parse
                               <*> parse
                               <*> parse
                               <*> parse
                               <*> parse
                               <*> parse
                               <*> parse
                               <*> parse

  store cptr (SHA512 h0 h1 h2 h3 h4 h5 h6 h7) =  runWrite cptr writeSHA512
    where writeSHA512 =  write h0
                      <> write h1
                      <> write h2
                      <> write h3
                      <> write h4
                      <> write h5
                      <> write h6
                      <> write h7

instance Primitive SHA512 where
  blockSize _ = BYTES 128
  {-# INLINE blockSize #-}
  newtype Cxt SHA512 = SHA512Cxt SHA512 deriving (Eq, Show, Storable)

instance SafePrimitive SHA512

instance HasPadding SHA512 where
  maxAdditionalBlocks _ = 1
  padLength = shaPadLength 16
  padding   = shaPadding   16

instance Default (Cxt SHA512) where
  def = SHA512Cxt $ SHA512 0x6a09e667f3bcc908
                          0xbb67ae8584caa73b
                          0x3c6ef372fe94f82b
                          0xa54ff53a5f1d36f1
                          0x510e527fade682d1
                          0x9b05688c2b3e6c1f
                          0x1f83d9abfb41bd6b
                          0x5be0cd19137e2179
