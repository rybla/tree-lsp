module Tlsp.Frontend.App where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
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
import Tlsp.Common (EditorState, Hello(..), Request(..), Response)
import Tlsp.Frontend.Common (style)
import Tlsp.Frontend.Console as Console
import Tlsp.Frontend.Spec (Frontend)
import Type.Prelude (Proxy(..))

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

  handleAction :: Action -> HM Unit
  handleAction Initialize = runHM' do
    request :: Request <- HelloRequest (Hello "this is a hello") # pure
    logConsole "test" (HH.text $ "fetch /tlsp/hello-goodbye <= " <> show request) # lift
    result <-
      fetch "/tlsp/hello-goodbye"
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString request
        } # liftAff
    str <- result.text # liftAff
    response :: _ Response <- fromJsonString str # pure
    logConsole "test" (HH.text $ "fetch /tlsp/hello-goodbye => " <> show response) # lift

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

