{-|

A cryptographic cipher abstraction.

-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}

module Raaz.Primitives.Cipher
       ( StreamGadget
       -- * Block Cipher Modes
       --
       -- A block cipher can be run in many different modes. These
       -- types capture the different modes of operation.
       , Mode(..)
       -- * Cipher gadget
       --
       -- A cipher that is a gadget should support both encryption and
       -- decryption. These mutually inverse operation are
       -- differentianted via a type argument.
       , Stage(..)
       ) where

import           Data.Typeable

import           Raaz.Primitives

-- | A block cipher can work in multiple modes. A mode describes how
-- to apply a single block operation securely on data spanning
-- multiple blocks.
data Mode = ECB
          | CBC
          | CTR
          | GCM
            deriving (Show,Typeable)

-- | A block cipher has two stages.
-- 1. Encryption - Encoding message into a ciphertext.
-- 2. Decryption - Decoding message from a ciphertext.
data Stage = Encryption
           | Decryption
           deriving (Show,Typeable)

-- A cipher gadget is one that supports both encryption and
-- decryption. For block ciphers, we do not take care of padding. In
-- fact there are no standard ways to pad messages and these are
-- usually application dependent.
-- class ( Gadget (g Encryption)
--       , Gadget (g Decryption)
--       , Initializable (PrimitiveOf (g Encryption))
--       , Initializable (PrimitiveOf (g Decryption))
--       ) => CipherGadget (g :: Stage -> *)


-- | This class captures gadgets which can be used as stream ciphers.
-- Any block cipher can also be seen as a stream cipher if it is run
-- in say counter mode.
class Gadget g => StreamGadget g
