{-

Provides Proxy types to be used in functions with kind restrictions

-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
module Raaz.Util.Proxy where

-- | Proxy data type with a dummy type variable
data Proxy a = Proxy

-- | Change from one proxy to another
reproxy :: Proxy a -> Proxy b
reproxy Proxy = Proxy
