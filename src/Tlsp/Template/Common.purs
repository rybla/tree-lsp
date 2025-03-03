module Tlsp.Template.Common where

import Prelude

import Effect (Effect)

type TemplateMain = { base_url :: String } -> Effect { name :: String }
