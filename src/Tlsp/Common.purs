module Tlsp.Common where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Tlsp.Data.Expr (Expr)
import Tlsp.Data.Tree (CursorSkeleton)
import Utility (todo)

--------------------------------------------------------------------------------

dist_dirpath = "./dist/"
public_dirpath = "./public/"

--------------------------------------------------------------------------------

data Request
  = HelloRequest Hello
  | OtherRequest

derive instance Generic Request _

instance Show Request where
  show x = genericShow x

instance EncodeJson Request where
  encodeJson x = genericEncodeJson x

instance DecodeJson Request where
  decodeJson x = genericDecodeJson x

data Response
  = GoodbyeResponse Goodbye
  | OtherResponse

derive instance Generic Response _

instance Show Response where
  show x = genericShow x

instance EncodeJson Response where
  encodeJson x = genericEncodeJson x

instance DecodeJson Response where
  decodeJson x = genericDecodeJson x

--------------------------------------------------------------------------------

class IsSymbol name <= BackendCapability (name :: Symbol) i o | name -> i o where
  toRequest :: i -> Request
  fromRequest :: Request -> Maybe i
  toResponse :: o -> Response
  fromResponse :: Response -> Maybe o

instance BackendCapability "hello-goodbye" Hello Goodbye where
  toRequest = HelloRequest

  fromRequest (HelloRequest x) = pure x
  fromRequest _ = none

  toResponse = GoodbyeResponse

  fromResponse (GoodbyeResponse x) = pure x
  fromResponse _ = none

newtype Hello = Hello String

derive instance Generic Hello _
derive instance Newtype Hello _
derive newtype instance Show Hello
derive newtype instance EncodeJson Hello
derive newtype instance DecodeJson Hello

newtype Goodbye = Goodbye String

derive instance Generic Goodbye _
derive instance Newtype Goodbye _
derive newtype instance Show Goodbye
derive newtype instance EncodeJson Goodbye
derive newtype instance DecodeJson Goodbye

--------------------------------------------------------------------------------

type EditorState =
  { cursor :: CursorSkeleton
  , expr :: Expr
  }

type ConsoleMessage = { label :: String, content :: PlainHTML }

