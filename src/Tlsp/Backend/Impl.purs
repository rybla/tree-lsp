module Tlsp.Backend.Impl where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Except (ExceptT, throwError)
import Data.Maybe (Maybe, maybe)
import Data.Symbol (reflectSymbol)
import Effect.Aff (Aff)
import Tlsp.Common (class BackendCapability, Goodbye(..), Hello(..), Request, Response, fromRequest, toResponse)
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------

class BackendCapability name i o <= BackendCapabilityImpl name i o | name -> i o where
  handleBackendCapability :: i -> ExceptT String Aff o

handleBackendCapability'
  :: forall @name i o
   . BackendCapabilityImpl name i o
  => String
  -> Maybe (Request -> ExceptT String Aff Response)
handleBackendCapability' name' = do
  let name = reflectSymbol (Proxy @name)
  guard $ name == name'
  pure \req -> do
    i <- fromRequest @name req # maybe (throwError $ "invalid request for capability: " <> name) pure
    res <- handleBackendCapability @name i
    pure $ toResponse @name res

--------------------------------------------------------------------------------

instance BackendCapabilityImpl "hello-goodbye" Hello Goodbye where
  handleBackendCapability (Hello str) = do
    pure $ Goodbye $ "Goodbye, " <> str

