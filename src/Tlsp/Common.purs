module Tlsp.Common where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (none)
import Halogen.HTML (PlainHTML)
import Tlsp.Data.Expr (Expr)
import Tlsp.Data.Tree (CursorSkeleton)
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------

dist_dirpath = "./dist/"
public_dirpath = "./public/"

--------------------------------------------------------------------------------

data Request
  = HelloRequest HelloInput
  | OtherRequest

derive instance Generic Request _

instance Show Request where
  show x = genericShow x

instance EncodeJson Request where
  encodeJson x = genericEncodeJson x

instance DecodeJson Request where
  decodeJson x = genericDecodeJson x

data Response
  = HelloResponse HelloOutput
  | OtherResponse

derive instance Generic Response _

instance Show Response where
  show x = genericShow x

instance EncodeJson Response where
  encodeJson x = genericEncodeJson x

instance DecodeJson Response where
  decodeJson x = genericDecodeJson x

--------------------------------------------------------------------------------

class (IsSymbol name, Show i, Show o) <= BackendCapability (name :: Symbol) i o | name -> i o where
  toRequest :: Proxy name -> i -> Request
  fromRequest :: Proxy name -> Request -> Maybe i
  toResponse :: Proxy name -> o -> Response
  fromResponse :: Proxy name -> Response -> Maybe o

--------------------------------------------------------------------------------

instance BackendCapability "Hello" HelloInput HelloOutput where
  toRequest _ = HelloRequest

  fromRequest _ (HelloRequest x) = pure x
  fromRequest _ _ = none

  toResponse _ = HelloResponse

  fromResponse _ (HelloResponse x) = pure x
  fromResponse _ _ = none

newtype HelloInput = HelloInput String

derive instance Generic HelloInput _
derive instance Newtype HelloInput _
derive newtype instance Show HelloInput
derive newtype instance EncodeJson HelloInput
derive newtype instance DecodeJson HelloInput

newtype HelloOutput = HelloOutput String

derive instance Generic HelloOutput _
derive instance Newtype HelloOutput _
derive newtype instance Show HelloOutput
derive newtype instance EncodeJson HelloOutput
derive newtype instance DecodeJson HelloOutput

--------------------------------------------------------------------------------

type EditorState =
  { cursor :: CursorSkeleton
  , expr :: Expr
  }

type ConsoleMessage = { label :: String, content :: PlainHTML }

