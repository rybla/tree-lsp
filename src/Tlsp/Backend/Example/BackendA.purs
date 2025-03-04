module Tlsp.Backend.Example.BackendA where

import Prelude

import Data.List as List
import Tlsp.Backend.Spec (Backend, BackendCapabilityHandler, mkBackendCapabilityHandler)
import Tlsp.Common (HelloInput(..), HelloOutput(..))
import Type.Proxy (Proxy(..))

backend :: Backend
backend =
  { name: "BackendA"
  , handlers: List.fromFoldable
      [ handleHello
      ]
  }

handleHello :: BackendCapabilityHandler
handleHello = mkBackendCapabilityHandler (Proxy @"Hello") \(HelloInput str) -> do
  pure $ HelloOutput $ "Hello, " <> str

