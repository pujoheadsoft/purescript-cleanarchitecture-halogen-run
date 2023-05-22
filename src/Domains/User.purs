module Domains.User where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype UserId = UserId Int

-- show, eq instance
derive instance Generic UserId _
instance Show UserId where
  show = genericShow
instance Eq UserId where
  eq = genericEq
