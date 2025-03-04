module Tlsp.Backend.Server where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
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
import Node.Path as Path
import Node.Stream as Stream
import Tlsp.Backend.Spec (Backend)
import Tlsp.Common (dist_dirpath)

host = "localhost"
port = 8100

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

    case method of
      "POST" -> case url of
        "/tlsp/test" -> do
          in_str <- getRequestBody in_msg
          Console.logShow { in_str }
          Stream.writeString out_stream UTF8 "hello from back to front" # void # liftEffect
        _ -> pure unit
      "GET" -> case url of
        _ | Just (filepath /\ contentType) <- from_url_to_local_file url -> do
          FS.exists filepath # liftEffect >>= case _ of
            false -> do
              Console.log $ "not found: " <> filepath
              ServerResonse.setStatusCode notFound_StatusCode res # liftEffect
            true -> do
              out_msg # OutgoingMessage.setHeader "Content-Type" contentType # liftEffect
              content <- FS.readTextFile UTF8 filepath # liftEffect
              Stream.writeString out_stream UTF8 content # void # liftEffect
        _ -> ServerResonse.setStatusCode unimplemented_StatusCode res # liftEffect
      _ -> ServerResonse.setStatusCode unimplemented_StatusCode res # liftEffect

    Stream.end out_stream # liftEffect

  server
    # toNetServer
    # once_ listeningH (Console.log $ "server hosted at " <> url_base)

  listenTcp (server # toNetServer) { host, port }

from_url_to_local_file :: String -> Maybe (String /\ String)
from_url_to_local_file url
  | Path.extname url == ".html" = Just $ (dist_dirpath <> url) /\ "text/html"
  | Just url' <- String.stripSuffix (Pattern "/") url = Just $ (dist_dirpath <> url <> "index.html") /\ "text/html"
  | Path.extname url == ".css" = Just $ (dist_dirpath <> url) /\ "text/css"
  | Path.extname url == ".json" = Just $ (dist_dirpath <> url) /\ "application/json"
  | Path.extname url == ".js" = Just $ (dist_dirpath <> url) /\ "application/js"
  | Path.extname url == ".png" = Just $ (dist_dirpath <> url) /\ "image/png"
  | Path.extname url == ".ico" = Just $ (dist_dirpath <> url) /\ "image/ico"
  | otherwise = Nothing

success_StatusCode = 200
notFound_StatusCode = 404
unimplemented_StatusCode = 501

foreign import inspect_in_msg :: IncomingMessage IMServer -> Effect Unit

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

