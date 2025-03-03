module Tlsp.Backend.Server where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_, once_)
import Node.FS.Sync as FS
import Node.HTTP (createServer)
import Node.HTTP.IncomingMessage as IncomingMessage
import Node.HTTP.OutgoingMessage as OutgoingMessage
import Node.HTTP.Server (requestH, toNetServer)
import Node.HTTP.ServerResponse as ServerResonse
import Node.HTTP.ServerResponse as ServerResponse
import Node.Net.Server (listenTcp, listeningH)
import Node.Path as Path
import Node.Stream as Stream
import Tlsp.Backend.Spec (Backend)
import Tlsp.Common (dist_dirpath)

host = "localhost"
port = 7000

url_base = "http://" <> host <> ":" <> show port

make_main :: Backend -> Effect Unit
make_main backend = do
  Console.log $ "backend: " <> backend.name
  server <- createServer

  server
    # toNetServer
    # once_ listeningH (Console.log $ "server hosted at " <> url_base)

  server
    # on_ requestH \in_msg res -> do
        ServerResonse.setStatusCode 200 res

        let out_msg = res # ServerResponse.toOutgoingMessage
        let out_stream = out_msg # OutgoingMessage.toWriteable
        let url = in_msg # IncomingMessage.url
        let method = in_msg # IncomingMessage.method
        Console.logShow { url, method }

        case method of
          "GET" -> case url of
            _ | Just (filepath /\ contentType) <- from_url_to_local_file url -> do
              FS.exists filepath >>= case _ of
                false -> do
                  Console.log $ "not found: " <> filepath
                  ServerResonse.setStatusCode 404 res
                true -> do
                  out_msg # OutgoingMessage.setHeader "Content-Type" contentType
                  content <- FS.readTextFile UTF8 filepath
                  Stream.writeString out_stream UTF8 content # void
              Stream.end out_stream
            _ -> pure unit
          _ -> pure unit
  listenTcp (server # toNetServer) { host, port }

from_url_to_local_file ∷ String → Maybe (String /\ String)
from_url_to_local_file url
  | Path.extname url == ".html" = Just $ (dist_dirpath <> url) /\ "text/html"
  | Just url' <- String.stripSuffix (Pattern "/") url = Just $ (dist_dirpath <> url <> "index.html") /\ "text/html"
  | Path.extname url == ".css" = Just $ (dist_dirpath <> url) /\ "text/css"
  | Path.extname url == ".json" = Just $ (dist_dirpath <> url) /\ "application/json"
  | Path.extname url == ".js" = Just $ (dist_dirpath <> url) /\ "application/js"
  | Path.extname url == ".png" = Just $ (dist_dirpath <> url) /\ "image/png"
  | Path.extname url == ".ico" = Just $ (dist_dirpath <> url) /\ "image/ico"
  | otherwise = Nothing

