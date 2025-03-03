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
import Node.HTTP.Types (IMServer, IncomingMessage)
import Node.Net.Server (listenTcp, listeningH)
import Node.Path as Path
import Node.Stream (closeH, dataH, dataHEither, dataHStr, endH, readableH)
import Node.Stream as Stream
import Tlsp.Backend.Spec (Backend)
import Tlsp.Common (dist_dirpath)
import Unsafe.Coerce (unsafeCoerce)

host = "localhost"
port = 8100

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
        -- inspect_in_msg in_msg
        let in_stream = in_msg # IncomingMessage.toReadable
        in_stream # on_ readableH do Console.log "in_stream: on readable"
        in_stream # on_ dataH \_ -> Console.log "in_stream: on dataH" -- X
        in_stream # on_ dataHStr \_ -> Console.log "in_stream: on dataHStr" -- X
        in_stream # on_ dataHEither \_ -> Console.log "in_stream: on dataHEither" -- X
        in_stream # on_ endH do Console.log "in_stream: on end"
        in_stream # on_ closeH do Console.log "in_stream: on close"

        -- response is successful unless marked as failure later
        ServerResonse.setStatusCode success_StatusCode res

        let out_msg = res # ServerResponse.toOutgoingMessage
        let out_stream = out_msg # OutgoingMessage.toWriteable
        let url = in_msg # IncomingMessage.url
        let method = in_msg # IncomingMessage.method

        Console.logShow { url, method }

        case method of
          "POST" -> case url of
            "/tlsp/test" -> do
              -- in_str <- Stream.readString in_stream UTF8
              -- Console.logShow { in_str }
              -- body
              -- in_buf <- Stream.read in_stream
              -- Console.logShow { in_buf: in_buf # map (const "<buf>") }
              void $ Stream.writeString out_stream UTF8 "hello from back to front "
            _ -> pure unit
          "GET" -> case url of
            _ | Just (filepath /\ contentType) <- from_url_to_local_file url -> do
              FS.exists filepath >>= case _ of
                false -> do
                  Console.log $ "not found: " <> filepath
                  ServerResonse.setStatusCode notFound_StatusCode res
                true -> do
                  out_msg # OutgoingMessage.setHeader "Content-Type" contentType
                  content <- FS.readTextFile UTF8 filepath
                  void $ Stream.writeString out_stream UTF8 content
            _ -> ServerResonse.setStatusCode unimplemented_StatusCode res
          _ -> ServerResonse.setStatusCode unimplemented_StatusCode res
        Stream.end out_stream
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

success_StatusCode = 200
notFound_StatusCode = 404
unimplemented_StatusCode = 501

foreign import inspect_in_msg :: IncomingMessage IMServer -> Effect Unit
