module Tlsp.Frontend.Console where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array (fold)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Halogen (liftEffect, modify_)
import Halogen as H
import Halogen.HTML (fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Properties as HP
import Tlsp.Common (ConsoleMessage)
import Tlsp.Frontend.Common (scrollIntoView, style)
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLElement as HTMLElement

data Query a = AddMessage ConsoleMessage a

type State =
  { messages :: Array ConsoleMessage
  }

data Action

type Output = Void

component ∷ forall input output. H.Component Query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State
  initialState _ =
    { messages: none
    }

  eval = H.mkEval H.defaultEval
    { handleQuery = handleQuery
    }

  handleQuery :: forall a. Query a -> _ (_ a)
  handleQuery (AddMessage m a) = do
    modify_ \state -> state { messages = state.messages `Array.snoc` m }
    pure (pure a)

  render state =
    HH.div
      [ style do
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ style do
              tell [ "padding: 0.5em" ]
              tell [ "background-color: black", "color: white" ]
          ]
          [ HH.text "Console" ]
      , HHK.div
          [ style do
              tell [ "overflow-y: scroll" ]
              tell [ "display: flex", "flex-direction: column" ]
          ] $ fold
          [ state.messages # mapWithIndex \i m ->
              Tuple (show i) $
                HH.div
                  [ style do
                      tell [ "display: flex", "flex-direction: row" ]
                  ]
                  [ HH.div
                      [ style do
                          tell [ "flex-grow: 0", "flex-shrink: 0" ]
                          tell [ "background-color: black", "color: white" ]
                          tell [ "padding: 0.5em" ]
                      ]
                      [ HH.text m.label ]
                  , HH.div
                      [ style do
                          tell [ "flex-grow: 1", "flex-shrink: 1" ]
                          tell [ "padding: 0.5em" ]
                      ]
                      [ m.content # fromPlainHTML ]
                  ]
          , [ let
                l = Array.length state.messages
              in
                Tuple (show l) $
                  HH.slot_ (Proxy @"scrollToMe") l scrollToMe_component unit
            ]
          ]

      ]

scrollToMe_component = H.mkComponent
  { initialState: const unit
  , eval: H.mkEval H.defaultEval
      { initialize = pure unit
      , handleAction = const do
          this # H.getHTMLElementRef >>= case _ of
            Nothing -> pure unit
            Just e -> e # HTMLElement.toElement # scrollIntoView # liftEffect
      }
  , render: const $ HH.div [ HP.ref this ] []
  }
  where
  this = H.RefLabel "this"

