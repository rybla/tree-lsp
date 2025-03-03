module Tlsp.Backend.Example.BackendA.Server where

import Prelude

import Effect (Effect)
import Tlsp.Backend.Example.BackendA (backend)
import Tlsp.Backend.Server (make_main)

main :: Effect Unit
main = make_main backend

