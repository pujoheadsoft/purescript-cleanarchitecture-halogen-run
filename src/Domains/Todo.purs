module Domains.Todo where

import Prelude

import Data.Array (filter)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype TodoTitle = TodoTitle String

data TodoStatus = Completed | InCompleted

newtype Todo = Todo {
  title :: TodoTitle,
  status :: TodoStatus
}

todo :: TodoTitle -> TodoStatus -> Todo
todo title status = Todo {title, status}

type Todos = Array Todo

completed :: Todos -> Todos
completed = filter \(Todo {status}) -> status == Completed

type Logics = {
  completed :: Todos -> Todos
}

logics :: Logics
logics = {
  completed: completed
}

-- show, eq instance
derive instance Generic Todo _
instance Show Todo where
  show = genericShow
instance Eq Todo where
  eq = genericEq

derive instance Generic TodoTitle _
instance Show TodoTitle where
  show = genericShow
instance Eq TodoTitle where
  eq = genericEq

derive instance Generic TodoStatus _
instance Show TodoStatus where
  show = genericShow
instance Eq TodoStatus where
  eq = genericEq
