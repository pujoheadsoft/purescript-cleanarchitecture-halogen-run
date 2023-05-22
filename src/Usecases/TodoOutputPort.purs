module Usecases.TodoOutputPort
  ( TODO_OUTPUT_PORT
  , TodoOutputPort(..)
  , TodoOutputPortType
  , _todoOutputPort
  , runOutputPort
  , setError
  , setTodos
  , todoOutputHandler
  )
  where

import Prelude

import Domains.Error (Error)
import Domains.Todo (Todos)
import Run (Run, interpret, lift, on, send)
import Run.State (STATE, put)
import State.State (TodoState)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type TodoOutputPortType = {
  stateForTodos :: Todos -> TodoState,
  stateForError :: Error -> TodoState
}

data TodoOutputPort a
  = SetTodos Todos a
  | SetError Error a

-- The following is almost boilerplate
derive instance todoOutputPortF :: Functor TodoOutputPort
type TODO_OUTPUT_PORT r = (todoOutputPort :: TodoOutputPort | r)
_todoOutputPort = Proxy :: Proxy "todoOutputPort"

setTodos :: forall r. Todos -> Run (TODO_OUTPUT_PORT + r) Unit
setTodos todos = lift _todoOutputPort (SetTodos todos unit)

setError :: forall r. Error -> Run (TODO_OUTPUT_PORT + r) Unit
setError error = lift _todoOutputPort (SetError error unit)

runOutputPort :: forall r. TodoOutputPortType -> Run (TODO_OUTPUT_PORT + STATE TodoState + r) ~> Run (STATE TodoState + r)
runOutputPort t run = interpret (on _todoOutputPort (todoOutputHandler t) send) run

todoOutputHandler :: forall r. TodoOutputPortType -> TodoOutputPort ~> Run (STATE TodoState + r)
todoOutputHandler t port = case port of
  SetTodos todos next -> do
    put $ t.stateForTodos todos
    pure next
  SetError e next -> do
    put $ t.stateForError e
    pure next
