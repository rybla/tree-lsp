module Tlsp.Backend.Spec where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.List (List)
import Effect.Aff (Aff)
import Tlsp.Common (class BackendCapability)
import Type.Prelude (Proxy)

type Backend =
  { name :: String
  , handlers :: List BackendCapabilityHandler
  }

newtype BackendCapabilityHandler = BackendCapabilityHandler (forall r. BackendCapabilityHandlerK r -> r)
type BackendCapabilityHandlerK r = forall name i o. BackendCapability name i o => Proxy name -> (i -> ExceptT String Aff o) -> r

mkBackendCapabilityHandler :: BackendCapabilityHandlerK BackendCapabilityHandler
mkBackendCapabilityHandler name handler = BackendCapabilityHandler \k -> k name handler

unBackendCapabilityHandler :: forall r. BackendCapabilityHandlerK r -> BackendCapabilityHandler -> r
unBackendCapabilityHandler k1 (BackendCapabilityHandler k2) = k2 k1

