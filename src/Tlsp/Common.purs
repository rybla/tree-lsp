module Tlsp.Common where

import Data.Either.Nested (type (\/))
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Tlsp.Data.Expr (Expr)
import Tlsp.Data.Tree (CursorSkeleton)
import Utility (todo)

--------------------------------------------------------------------------------

dist_dirpath = "./dist/"
public_dirpath = "./public/"

--------------------------------------------------------------------------------

-- class IsRequest i o | i -> o where 
--   from_request_url :: 

-- data Request :: Type -> Type -> Type
-- data Request a b = Request

-- request :: forall a b. Request a b -> a -> Aff (PlainHTML \/ b)
-- request = todo ""

--------------------------------------------------------------------------------

type EditorState =
  { cursor :: CursorSkeleton
  , expr :: Expr
  }

type ConsoleMessage = { label :: String, content :: PlainHTML }