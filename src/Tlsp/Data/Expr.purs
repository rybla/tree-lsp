module Tlsp.Data.Expr where

import Data.Maybe (Maybe)
import Data.Unfoldable (none)
import Tlsp.Data.Tree (CursorSkeleton, Tree(..))

--------------------------------------------------------------------------------

type Expr = Tree (Record FullNode)

holeExpr = Tree { value: HoleNode, ui: none } [] :: Expr

--------------------------------------------------------------------------------

type FullNode = BasicNode (UiNode ())

type UiNode node = (ui :: Maybe UiValue | node)

data UiValue = CursorNode

type BasicNode node = (value :: NodeValue | node)

data NodeValue
  = StringNode String
  | HoleNode

--------------------------------------------------------------------------------

type Cursor = CursorSkeleton

