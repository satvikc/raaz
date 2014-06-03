module Raaz.Primitives.Application
       ( Packet(..)
       , PacketSize(..)
       , Stack(..), stack
       , Application(..)
       , Hook(..), hook
       ) where

import Control.Monad.Trans

import Raaz.Types


-- | A protocol can wrap a buffer inside some envelope. For example,
-- Transport Layer of SSH protocol wraps the Payload as - [Payload
-- size, Padding Size, Payload, Padding , Mac]. The user should make
-- sure that enough space is available (on both sides of the payload)
-- on the buffer to wrap the envelop.
--
-- Note that no memory allocation is done in wrapping or unwrapping
-- and the same location is shared amongst payload and outer buffer.
-- This abstraction is defined to share the same buffer amonst
-- stacked protocols.
data Packet m envelop = Packet { wrap :: envelop
                                      -> CryptoBuffer
                                      -> m CryptoBuffer
                               , unwrap :: CryptoBuffer
                                        -> m (envelop, CryptoBuffer)
                               }

-- | Captures the size requirement of the envelop on the buffer. This
-- is used when you want to allocate a buffer to support the given
-- size of payload with this envelop.
class PacketSize envelop where
  -- | The maximum size, envelop of this type can take on the left of
  -- the payload.
  leftMaximum :: envelop -> BYTES Int
  -- | The maximum size, envelop of this type can take on the right of
  -- the payload.
  rightMaximum :: envelop -> BYTES Int

  -- | The size, this envelop will take on the left of the payload.
  leftSize :: envelop -> BYTES Int

  -- | The size, this envelop will take on the right of the payload.
  rightSize :: envelop -> BYTES Int

-- | Application interface which has three components.
--
-- * Receiving payload
-- * Sending payload
-- * Application completion
--
-- The user just need to define this for the given application.
--
-- Every application has some internal state which needs to captured
-- in the monad @m@. For example, ssh needs to keep track of the
-- handshake parameters like cipher, mac etc. which constitute the ssh
-- context. These can be kept in a state monad.
--
-- Note that the `Packet` interface for a given envelop is needed
-- while running the application to wrap and unwrap the buffer and
-- envelop.
data Application m envelop = Receive (envelop -> CryptoBuffer -> m (Application m envelop))
                           | Send (m (envelop, CryptoBuffer, Application m envelop))
                           | Done



-- | The application programmer needs to define a hook for any
-- protocol. The main purpose of the hook is to capture how the
-- envelope is handeled. Thus we can define custom hooks for each
-- protocol like SSH hook, TLS hook etc. Once we have the hook for a
-- given protocol, we can plug any custom application on this hook to
-- run that application using the given protocol. For example, we can
-- define an application for vpn which doesn't know anything about ssh
-- and then we can plug the vpn application to the ssh hook to get vpn
-- through ssh.
data Hook m envelop = Hook { updateState :: envelop -> m ()
                           , fetchState :: m envelop
                           }

-- | Combine an `Application` to a `Hook`. Remember that you can use
-- `stack`, to get the corresponding `Packet` interface for the create
-- `Application.
hook :: ( Monad m, MonadTrans t, Monad (t m) )
     => Application m envelop
     -> Hook (t m) envelop'
     -> Application (t m) (Stack envelop' envelop)
hook Done _ = Done
hook (Receive f) hk = Receive f'
  where
    f' (Stack envelop' envelop) buffer = do
      updateState hk envelop'
      app <- lift $ f envelop buffer
      return $ hook app hk
hook (Send act) hk = Send act'
  where
    act' = do
      (envelop,buffer,app) <- lift act
      envelop' <- fetchState hk
      return ((Stack envelop' envelop), buffer, hook app hk)

-- | Stacking packets. @envelop a@ is on top of @envelop b@.
data Stack a b = Stack a b

-- | Stack a packet on top of another. This is like putting a envelope
-- inside another. Through this stacking you can unwrap and wrap both
-- envelopes together.
stack :: ( Monad m, MonadTrans t, Monad (t m) )
      => Packet (t m) e2
      -> Packet m e1
      -> Packet (t m) (Stack e2 e1)
stack (Packet we2 uwe2) (Packet we1 uwe1) = Packet we1e2 uwe1e2
  where
    we1e2 (Stack e2 e1) buff = do
      buff' <- lift $ we1 e1 buff
      we2 e2 buff'
    uwe1e2 buff = do
      (e2,buff') <- uwe2 buff
      (e1,buff'') <- lift $ uwe1 buff'
      return (Stack e2 e1, buff'')
