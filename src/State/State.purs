module State.State where

import Prelude

import Data.Maybe (Maybe)

type Todo = { title :: String }

type TodoState = { 
  todos :: Array Todo,
  errorMessage :: Maybe String
}