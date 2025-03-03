module Tlsp.Node where

import Prelude

import Effect (Effect)
import Node.Path (FilePath)

foreign import rmDir :: FilePath -> Effect Unit

foreign import copyDir :: { source :: FilePath, target :: FilePath } -> Effect Unit

