module Domains.User where

import Prelude

newtype UserId = UserId Int

-- show, eq instance
derive newtype instance showUserId :: Show UserId
derive newtype instance eqUserId :: Eq UserId