module Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Warn, Text)

todo :: forall a. Warn (Text "contains TODOs") => String -> a
todo msg = unsafeCrashWith $ "[[TODO]]\n" <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[[BUG]]\n" <> msg

impossible :: forall @a. Unit -> a
impossible _ = bug "impossible"

