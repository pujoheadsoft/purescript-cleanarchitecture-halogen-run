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
derive newtype instance showTodo :: Show Todo
derive newtype instance eqTodo :: Eq Todo

derive newtype instance showTitle :: Show TodoTitle
derive newtype instance eqTitle :: Eq TodoTitle

derive instance Generic TodoStatus _
instance Show TodoStatus where
  show = genericShow
instance Eq TodoStatus where
  eq = genericEq
