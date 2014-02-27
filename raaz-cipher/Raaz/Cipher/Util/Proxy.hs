{-

Provides Proxy types to be used with restricted kinds. We are not
using PolyKinds because of a bug in ghc7.4.

-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Raaz.Cipher.Util.Proxy where

import Raaz.Primitives.Cipher

-- | Proxy data type for Cipher Mode
data ProxyMode (a :: Mode) = ProxyMode

-- | Proxy data type for Cipher Stage
data ProxyStage (a :: Stage) = ProxyStage
