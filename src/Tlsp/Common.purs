module Tlsp.Common where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)

data Tree a = Tree a (Array (Tree a))

data PathI = PathI (List Int)

derive instance Generic PathI _

data CursorI
  = PointI PathI
  | SelectI PathI PathI

derive instance Generic CursorI _

