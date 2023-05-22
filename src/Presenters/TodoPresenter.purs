module Presenters.TodoPresenter where

import Prelude

import Data.Maybe (Maybe(..))
import Domains.Error (Error(..))
import Domains.Todo (Todo(..), TodoTitle(..), Todos)
import State.State (TodoState)
import Usecases.TodoOutputPort (TodoOutputPortType)

createOutputPort :: TodoOutputPortType
createOutputPort = {
  stateForTodos: stateForTodos,
  stateForError: stateForError
}

stateForTodos :: Todos -> TodoState
stateForTodos values = {
  todos: values <#> \(Todo {title: TodoTitle t}) -> {title: t},
  errorMessage: Nothing
}

stateForError :: Error -> TodoState
stateForError (Error e) = {
  todos: [],
  errorMessage: Just e
}
