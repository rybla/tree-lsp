module Tlsp.Frontend.App where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD
import Tlsp.Frontend.Common (style)
import Tlsp.Frontend.Spec (Frontend)

make_main :: Frontend -> Effect Unit
make_main frontend = HA.runHalogenAff (HVD.runUI component frontend =<< HA.awaitBody)

component :: forall query output m. MonadAff m => H.Component query Frontend output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState frontend =
    { frontend
    }

  eval = H.mkEval H.defaultEval

  render state =
    HH.div
      [ style do tell [ "display: flex", "flex-direction: column" ] ]
      [ HH.div
          [ style do
              tell [ "padding: 1em" ]
              tell [ "background-color: black", "color: white" ]
          ]
          [ HH.text $ "Frontend Name: " <> state.frontend.name ]
      , HH.div
          [ style do tell [ "padding: 1em" ] ]
          [ HH.text "{{editor}}" ]
      ]

