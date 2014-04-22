{-|

The Sha512 hash.

-}

module Raaz.Hash.Sha512
       ( SHA512, sha512
       , sha512File, sourceSha512
       -- * Encoding as binary/hexadecimal
       , toByteString, toHex
       ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Raaz.ByteSource
import Raaz.Crypto.Hash( sourceHash, hash, hashFile)
import Raaz.Hash.Sha512.Type(SHA512)
import Raaz.Hash.Sha512.Instance()
import Raaz.Types           ( toByteString               )
import Raaz.Util.ByteString ( toHex                      )


-- | Compute the sha512 hash of the given byte source.
sourceSha512 :: ByteSource src => src -> IO SHA512
sourceSha512 = sourceHash

-- | Compute the sha512 hash of a file.
sha512File   :: FilePath -> IO SHA512
sha512File   = hashFile

-- | Compute the sha512 hash of a pure byte source.
sha512       :: PureByteSource src => src -> SHA512
sha512       = hash
{-# SPECIALIZE sha512 :: B.ByteString -> SHA512 #-}
{-# SPECIALIZE sha512 :: L.ByteString -> SHA512 #-}
