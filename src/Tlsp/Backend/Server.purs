module Tlsp.Backend.Server where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.EventEmitter (once_)
import Node.FS.Sync as FS
import Node.HTTP.IncomingMessage as IncomingMessage
import Node.HTTP.OutgoingMessage as OutgoingMessage
import Node.HTTP.Server (toNetServer)
import Node.HTTP.ServerResponse as ServerResonse
import Node.HTTP.ServerResponse as ServerResponse
import Node.HTTP.Types (HttpServer, IMServer, IncomingMessage, ServerResponse)
import Node.Net.Server (listenTcp, listeningH)
import Node.Path (FilePath)
import Node.Stream as Stream
import Tlsp.Backend.Impl (handleBackendCapability')
import Tlsp.Backend.Spec (Backend)
import Tlsp.Common as Tlsp
import Utility (format)

host = "localhost"
port = 8000

url_base = "http://" <> host <> ":" <> show port

make_main :: Backend -> Effect Unit
make_main backend = do
  Console.log $ "backend: " <> backend.name

  server <- createServer_simplified \in_msg res -> do
    -- response is successful unless marked as failure later
    ServerResonse.setStatusCode success_StatusCode res # liftEffect

    let out_msg = res # ServerResponse.toOutgoingMessage
    let out_stream = out_msg # OutgoingMessage.toWriteable
    let url = in_msg # IncomingMessage.url
    let method = in_msg # IncomingMessage.method
    Console.logShow { url, method }

    result <- runExceptT do
      case method /\ url of
        "POST" /\ "/hello" -> do
          Stream.writeString out_stream UTF8 "hello from back to front" # void >>> liftEffect

        "POST" /\ _ | Just name <- stripPrefix (Pattern "/tlsp/") url -> do

          in_str <- getRequestBody in_msg # liftAff >>= flip either pure \err -> do
            Stream.writeString out_stream UTF8
              ("Encountered error when reading request body: {{err}}" # format { err })
              # void >>> liftEffect
            throwError serverError_StatusCode

          Console.logShow { in_str }

          request :: Tlsp.Request <- fromJsonString in_str # flip either pure \err -> do
            Stream.writeString out_stream UTF8
              ("Encountered error when parsing request: {{err}}" # format { err: printJsonDecodeError err })
              # void >>> liftEffect
            throwError serverError_StatusCode

          response :: Tlsp.Response <- handleRequest name request # runExceptT # liftAff >>= flip either pure \err -> do
            Stream.writeString out_stream UTF8
              ("Encountered error when handling request: {{err}}" # format { err })
              # void >>> liftEffect
            throwError serverError_StatusCode

          Stream.writeString out_stream UTF8 (toJsonString response) # void >>> liftEffect

          pure unit

        "GET" /\ "/hello" -> do
          Stream.writeString out_stream UTF8 "hello from back to front" # void >>> liftEffect

        "GET" /\ _ -> do
          let { filepath, contentType } = from_url_to_resource url
          whenM (not <$> FS.exists filepath # liftEffect) do
            Console.log $ "not found: " <> filepath
            throwError notFound_StatusCode
          out_msg # OutgoingMessage.setHeader "Content-Type" contentType # liftEffect
          content <- FS.readTextFile UTF8 filepath # liftEffect
          Stream.writeString out_stream UTF8 content # void >>> liftEffect

        _ -> throwError unimplemented_StatusCode

    case result of
      Left statusCode -> ServerResonse.setStatusCode statusCode res # liftEffect
      Right _ -> pure unit

    Stream.end out_stream # liftEffect

  server
    # toNetServer
    # once_ listeningH (Console.log $ "server hosted at " <> url_base)

  listenTcp (server # toNetServer) { host, port }

--------------------------------------------------------------------------------

handleRequest :: String -> Tlsp.Request -> ExceptT String Aff Tlsp.Response
handleRequest name req = go capabilities
  where
  go :: List (String -> Maybe (Tlsp.Request -> ExceptT String Aff Tlsp.Response)) -> ExceptT String Aff Tlsp.Response
  go Nil = throwError $ "unimplemented request: " <> toJsonString req
  go (k : mks) = case k name of
    Nothing -> go mks
    Just k' -> k' req

  capabilities = List.fromFoldable
    [ handleBackendCapability' @"hello-goodbye"
    ]

--------------------------------------------------------------------------------

from_url_to_resource :: String -> { filepath :: FilePath, contentType :: String }
from_url_to_resource url
  | String.stripSuffix (Pattern "/") url # isJust =
      from_url_to_resource (url <> "index.html")
from_url_to_resource url = { filepath, contentType: getType filepath }
  where
  filepath = Tlsp.dist_dirpath <> url

--------------------------------------------------------------------------------

success_StatusCode = 200
notFound_StatusCode = 404
serverError_StatusCode = 500
unimplemented_StatusCode = 501

foreign import createServer_simplified_
  :: (IncomingMessage IMServer -> ServerResponse -> Effect (Promise Unit))
  -> Effect HttpServer

createServer_simplified
  :: (IncomingMessage IMServer -> ServerResponse -> Aff Unit)
  -> Effect HttpServer
createServer_simplified requestListener =
  createServer_simplified_ (\req res -> requestListener req res # fromAff)

foreign import getRequestBody_
  :: { ok :: String -> String \/ String, error :: String -> String \/ String }
  -> IncomingMessage IMServer
  -> Effect (Promise (String \/ String))

getRequestBody :: IncomingMessage IMServer -> Aff (String \/ String)
getRequestBody req = getRequestBody_ { ok: pure, error: throwError } req # toAffE

foreign import getType :: FilePath -> String
