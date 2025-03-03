module Tlsp.Frontend.App where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Writer (tell)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch (Method(..), fetch)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD
import JS.Fetch.RequestMode (RequestMode(..))
import Tlsp.Common (EditorState)
import Tlsp.Frontend.Common (style)
import Tlsp.Frontend.Console as Console
import Tlsp.Frontend.Spec (Frontend)
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------

make_main :: Frontend -> Effect Unit
make_main frontend = HA.runHalogenAff (HVD.runUI component frontend =<< HA.awaitBody)

--------------------------------------------------------------------------------

type HM' = ExceptT PlainHTML HM

runHM' :: forall a. HM' Unit -> HM Unit
runHM' m = runExceptT m >>= case _ of
  Left err -> logConsole "error" err
  Right _ -> pure unit

type HM = H.HalogenM State Action Slots Output M

type M = Aff

type State =
  { frontend :: Frontend
  , editor :: Maybe EditorState
  }

data Action =
  Initialize

type Slots =
  ( console :: H.Slot Console.Query Void Unit
  )

type Output = Void

--------------------------------------------------------------------------------

component :: forall query. H.Component query Frontend Output M
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State
  initialState frontend =
    { frontend
    , editor: none
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = handleAction
    }

  handleAction :: Action -> HM Unit
  handleAction Initialize = do
    logConsole "test" $ HH.text $ "begin fetch /tlsp/test"
    { text } <-
      fetch "/tlsp/test"
        { method: POST
        , mode: SameOrigin
        , headers: { "Content-Type": "text/plain" }
        , body: "hello from front to back"
        } # liftAff
    logConsole "test" $ HH.text $ "end fetch /tlsp/test"
    str <- text # liftAff
    logConsole "test" $ HH.text $ "fetch /tlsp/test response: " <> str
    -- response <- request ?a ?a # liftAff
    pure unit

  render state =
    HH.div
      [ style do tell [ "display: flex", "flex-direction: column" ] ]
      [ HH.div
          [ style do
              tell [ "padding: 0.5em" ]
              tell [ "background-color: black", "color: white" ]
          ]
          [ HH.text $ "frontend: " <> state.frontend.name ]
      , HH.div
          [ style do tell [ "padding: 0.5em" ] ]
          [ HH.text "{{editor}}" ]
      , HH.slot_ (Proxy @"console") unit Console.component unit
      ]

--------------------------------------------------------------------------------

logConsole label content = H.tell (Proxy @"console") unit $ Console.AddMessage { label, content }