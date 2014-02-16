{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Raaz.Cipher.AES.Internal
       ( module Raaz.Cipher.AES.Ref.Type
       , module Raaz.Cipher.Util.Typeable
       , Ref128(..)
       , Ref192(..)
       , Ref256(..)
       , CPortable128(..)
       , CPortable192(..)
       , CPortable256(..)
       , AES128(..)
       , AES192(..)
       , AES256(..)
       ) where

import Raaz.Cipher.AES.Ref.Type

import Data.Typeable
import Raaz.Memory
import Raaz.Primitives.Cipher
import Raaz.Util.Proxy

import Raaz.Cipher.Util.Typeable

-- | Reference Implementation for AES128 in CBC Mode
data Ref128 (m :: Mode) (s :: Stage) = Ref128 (CryptoCell Expanded128, CryptoCell STATE)

-- | Reference Implementation for AES192 in CBC Mode
data Ref192 (m :: Mode) (s :: Stage) = Ref192 (CryptoCell Expanded192, CryptoCell STATE)

-- | Reference Implementation for AES256 in CBC Mode
data Ref256 (m :: Mode) (s :: Stage) = Ref256 (CryptoCell Expanded256, CryptoCell STATE)

-- | CPortable Implementation for AES128 in CBC Mode
data CPortable128 (m :: Mode) (s :: Stage) = CPortable128 (CryptoCell Expanded128, CryptoCell STATE)

-- | CPortable Implementation for AES192 in CBC Mode
data CPortable192 (m :: Mode) (s :: Stage) = CPortable192 (CryptoCell Expanded192, CryptoCell STATE)

-- | CPortable Implementation for AES256 in CBC Mode
data CPortable256 (m :: Mode) (s :: Stage) = CPortable256 (CryptoCell Expanded256, CryptoCell STATE)

data AES128 (m :: Mode) (s :: Stage) = AES128 deriving (Show,Eq)

data AES192 (m :: Mode) (s :: Stage) = AES192 deriving (Show,Eq)

data AES256 (m :: Mode) (s :: Stage) = AES256 deriving (Show,Eq)

instance (TypeableMode m, TypeableStage s) => Typeable (AES128 m s) where
  typeOf _ =
    mkTyConApp
    (mkTyCon3 "raaz-cipher" "Raaz.Cipher.Internal" "AES128")
        [modeRep (Proxy :: Proxy m), stageRep (Proxy :: Proxy s)]

instance (TypeableMode m, TypeableStage s) => Typeable (AES192 m s) where
  typeOf _ =
    mkTyConApp
    (mkTyCon3 "raaz-cipher" "Raaz.Cipher.Internal" "AES192")
        [modeRep (Proxy :: Proxy m), stageRep (Proxy :: Proxy s)]

instance (TypeableMode m, TypeableStage s) => Typeable (AES256 m s) where
  typeOf _ =
    mkTyConApp
    (mkTyCon3 "raaz-cipher" "Raaz.Cipher.Internal" "AES256")
        [modeRep (Proxy :: Proxy m), stageRep (Proxy :: Proxy s)]
