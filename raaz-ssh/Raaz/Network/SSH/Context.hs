{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Raaz.Network.SSH.Context (Context(..)) where

import Raaz.Memory
import Raaz.Primitives.Cipher
import Raaz.Random

import Raaz.Network.SSH.Backend

-- | It captures a connection state.
data Context where
  Context :: (Bufferable a,Backend b,CipherGadget g0,CipherGadget g1,StreamGadget r)
          => Buffer a               -- ^ Buffer
          -> b                      -- ^ Backend
          -> Maybe (g0 Encryption)  -- ^ Encryption Gadget
          -> Maybe (g1 Decryption)  -- ^ Decryption Gadget
          -> RandomSource r         -- ^ Random Number Generator
          ->  Context
