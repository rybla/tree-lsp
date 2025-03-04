module Tlsp.Frontend.App where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Fetch (Method(..), fetch)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD
import Tlsp.Common (class BackendCapability, EditorState, Hello(..), Request(..), Response, fromResponse, toRequest)
import Tlsp.Frontend.Common (style)
import Tlsp.Frontend.Console as Console
import Tlsp.Frontend.Spec (Frontend)
import Type.Prelude (Proxy(..), reflectSymbol)
import Utility (format, todo)

--------------------------------------------------------------------------------

make_main :: Frontend -> Effect Unit
make_main frontend = HA.runHalogenAff (HVD.runUI component frontend =<< HA.awaitBody)

--------------------------------------------------------------------------------

type HM' = ExceptT PlainHTML HM

runHM' :: HM' Unit -> HM Unit
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

  requestBackendCapability :: forall @name i o. BackendCapability name i o => i -> HM' o
  requestBackendCapability i = do
    let name = reflectSymbol (Proxy @name)
    logConsole "test" (code $ "fetch /tlsp/{{name}} <== {{i}}" # format { name, i: show i }) # lift

    let request = toRequest @name i
    result <-
      fetch ("/tlsp/" <> name)
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString request
        } # liftAff
    body <- result.text # liftAff
    when (not result.ok) do throwError $ HH.span_ [ text "in ", code "requestBackendCapability", text ", bad result: ", code (show { statusText: result.statusText, body }) ]
    response <- fromJsonString body # flip either pure \err -> throwError $ HH.span_ [ text "in ", code "requestBackendCapability", text ", failed to decode body: ", code (show { err: printJsonDecodeError err, body }) ]
    o <- fromResponse @name response # flip maybe pure do throwError $ HH.span_ [ text "in ", code "requestBackendCapability", text ", response is wrong form: ", code (show response) ]

    logConsole "test" (code $ "fetch /tlsp/{{name}} ==> {{o}}" # format { name, o: show o }) # lift
    pure o

  handleAction :: Action -> HM Unit
  handleAction Initialize = runHM' do
    _ <- requestBackendCapability @"hello-goodbye" $ Hello "Henry"
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

logConsole :: String -> PlainHTML -> HM Unit
logConsole label content = H.tell (Proxy @"console") unit $ Console.AddMessage { label, content }

code str = HH.code_ [ HH.text str ]
text str = HH.text str