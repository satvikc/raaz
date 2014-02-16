{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Raaz.Cipher.Util.Typeable where

import Data.Typeable
import Raaz.Primitives.Cipher
import Raaz.Util.Proxy

class TypeableMode (m :: Mode) where
  modeRep :: Proxy m -> TypeRep

instance TypeableMode ECB where
  modeRep _ = mkTyConApp (mkTyCon3 "raaz-primitives" "Raaz.Primitives.Cipher" "ECB") []

instance TypeableMode CBC where
  modeRep _ = mkTyConApp (mkTyCon3 "raaz-primitives" "Raaz.Primitives.Cipher" "CBC") []

instance TypeableMode CTR where
  modeRep _ = mkTyConApp (mkTyCon3 "raaz-primitives" "Raaz.Primitives.Cipher" "CTR") []

instance TypeableMode GCM where
  modeRep _ = mkTyConApp (mkTyCon3 "raaz-primitives" "Raaz.Primitives.Cipher" "GCM") []


class TypeableStage (s :: Stage) where
  stageRep :: Proxy s -> TypeRep

instance TypeableStage Encryption where
  stageRep _ = mkTyConApp (mkTyCon3 "raaz-primitives" "Raaz.Primitives.Cipher" "Encryption") []

instance TypeableStage Decryption where
  stageRep _ = mkTyConApp (mkTyCon3 "raaz-primitives" "Raaz.Primitives.Cipher" "Decryption") []
