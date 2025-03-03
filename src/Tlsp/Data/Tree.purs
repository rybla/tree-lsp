module Tlsp.Data.Tree where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)

--------------------------------------------------------------------------------

data Tree a = Tree a (Array (Tree a))

--------------------------------------------------------------------------------

data TreeDiffSkeleton

--------------------------------------------------------------------------------

newtype PathSkeleton = PathSkeleton (List Int)

derive instance Generic PathSkeleton _

derive instance Newtype PathSkeleton _

derive newtype instance Semigroup PathSkeleton

-- TODO: does this append in the right direction?
derive newtype instance Monoid PathSkeleton

--------------------------------------------------------------------------------

data CursorSkeleton
  = PathCursorSkeleton PathSkeleton
  | SelectCursorSkeleton PathSkeleton PathSkeleton

derive instance Generic CursorSkeleton _

