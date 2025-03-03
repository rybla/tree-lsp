module Tlsp.Frontend.Common where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Data.Array as Array
import Effect (Effect)
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Web.DOM (Element)

style :: forall w i. Writer (Array String) Unit -> IProp (style :: String | w) i
style w = HP.style $ Array.intercalate "; " $ execWriter w

foreign import scrollIntoView :: Element -> Effect Unit

