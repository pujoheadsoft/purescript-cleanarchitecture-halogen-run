module Domains.Error where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Error = Error String

derive instance Generic Error _
instance Show Error where
  show = genericShow
instance Eq Error where
  eq = genericEq
