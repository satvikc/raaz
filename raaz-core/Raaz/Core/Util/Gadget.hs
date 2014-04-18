{-

This module contains some abstractions built on top of gadgets

-}

module Raaz.Core.Util.Gadget ( applyOnByteSource )where

import Raaz.Core.Types.ByteSource
import Raaz.Core.Crypto
import Raaz.Core.Types
import Raaz.Core.Util.Ptr

-- | Apply the given gadget on the bytesource. This function is
-- different from transformGadget as it assumes that number of bytes
-- in the source is in the multiple of block size of the primitive.
applyOnByteSource :: ( ByteSource src
                     , Gadget g )
                     => g     -- ^ Gadget
                     -> src   -- ^ The byte source
                     -> IO ()
applyOnByteSource g src = allocaBuffer nBlocks $ go src
  where nBlocks = recommendedBlocks g
        go source cptr =   fill nBlocks source cptr
                             >>= withFillResult continue endIt
           where continue rest = do apply g nBlocks cptr
                                    go rest cptr
                 endIt r       = apply g (cryptoCoerce r) cptr
