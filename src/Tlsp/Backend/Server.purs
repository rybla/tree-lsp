module Tlsp.Backend.Server where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_, once_)
import Node.HTTP (createServer)
import Node.HTTP.IncomingMessage as IncomingMessage
import Node.HTTP.OutgoingMessage as OutgoingMessage
import Node.HTTP.Server (requestH, toNetServer)
import Node.HTTP.ServerResponse as ServerResponse
import Node.Net.Server (listenTcp, listeningH)
import Node.Stream as Stream
import Tlsp.Backend.Spec (Backend)

host = "localhost"
port = 8000

uri_base = "http://" <> host <> ":" <> show port

make_main :: Backend -> Effect Unit
make_main backend = do
  Console.log $ "Backend Name: " <> backend.name
  server <- createServer

  server
    # toNetServer
    # once_ listeningH (Console.log $ "server hosted at " <> uri_base)

  server
    # on_ requestH \in_msg res -> do
        let out_msg = res # ServerResponse.toOutgoingMessage
        let out_stream = out_msg # OutgoingMessage.toWriteable
        let url = in_msg # IncomingMessage.url
        let method = in_msg # IncomingMessage.method
        Console.logShow { url, method }
        case method of
          "GET" -> case url of
            _ -> do
              out_msg # OutgoingMessage.setHeader "Content-Type" "application/json"
              Stream.writeString out_stream UTF8 (show { url, backend_end: backend.name }) # void
              Stream.end out_stream
          _ -> pure unit
        pure unit

  listenTcp (server # toNetServer) { host, port }

