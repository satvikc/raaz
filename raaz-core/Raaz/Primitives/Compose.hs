{-

Module to compose primitives and gadgets.

-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module Raaz.Primitives.Compose ( Compose(..) ) where

import Control.Applicative

import Raaz.Primitives
import Raaz.Types

-- | Composition of `Primitive`s or `Gadget`s.
data Compose a b = Compose a b deriving Show

instance ( SafePrimitive a
         , SafePrimitive b
         ) => Primitive (Compose a b) where
  -- | Block size of the composition is the lcm of individual
  -- blocksizes.
  blockSize (Compose a b) = lcm (blockSize a) (blockSize b)

  data Cxt (Compose a b) = ComposeCxt (Cxt a) (Cxt b)

instance ( SafePrimitive a
         , SafePrimitive b
         ) => SafePrimitive (Compose a b) where


instance ( SafePrimitive a
         , SafePrimitive b
         , Digestible a
         , Digestible b
         ) => Digestible (Compose a b) where

  type Digest (Compose a b) = Compose (Digest a) (Digest b)

  toDigest (ComposeCxt cxta cxtb) = Compose (toDigest cxta) (toDigest cxtb)

instance ( Gadget g1
         , Gadget g2
         , SafePrimitive (PrimitiveOf g1)
         , SafePrimitive (PrimitiveOf g2)
         ) => Gadget (Compose g1 g2) where

  type PrimitiveOf (Compose g1 g2) = Compose (PrimitiveOf g1) (PrimitiveOf g2)

  type MemoryOf (Compose g1 g2) = (MemoryOf g1, MemoryOf g2)

  newGadgetWithMemory (mem1, mem2) = Compose <$> newGadgetWithMemory mem1
                                             <*> newGadgetWithMemory mem2

  initialize (Compose g1 g2) (ComposeCxt cxt1 cxt2) =  initialize g1 cxt1
                                                    >> initialize g2 cxt2

  finalize (Compose g1 g2) = ComposeCxt <$> finalize g1 <*> finalize g2

  recommendedBlocks (Compose g1 g2) = cryptoCoerce bytes
    where
      bytes :: BYTES Int
      bytes = lcm (cryptoCoerce $ recommendedBlocks g1)
                  (cryptoCoerce $ recommendedBlocks g2)


  apply (Compose g1 g2) blks cptr =  apply g1 blk1 cptr
                                  >> apply g2 blk2 cptr
    where
      bytes :: BYTES Int
      bytes = cryptoCoerce blks
      blk1 = cryptoCoerce bytes
      blk2 = cryptoCoerce bytes
