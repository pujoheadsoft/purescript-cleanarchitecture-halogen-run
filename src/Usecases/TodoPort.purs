module Usecases.TodoPort where

import Prelude

import Data.Either (Either(..))
import Domains.Error (Error)
import Domains.Todo (Todos)
import Domains.User (UserId)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Run (AFF, Run, interpret, lift, liftAff, on, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type TodoPortType = {
  findTodos :: UserId -> Aff (Either Error Todos)
}

data TodoPort a
  = FindTodos UserId ((Either Error Todos) -> a)

-- The following is almost boilerplate
derive instance todoPortF :: Functor TodoPort
type TODO_PORT r = (todoPort :: TodoPort | r)
_todoPort = Proxy :: Proxy "todoPort"

findTodos :: forall r. UserId -> Run (TODO_PORT + r) (Either Error Todos)
findTodos userId = lift _todoPort (FindTodos userId identity)

runPort :: forall r. TodoPortType -> Run (TODO_PORT + AFF + r) ~> Run (AFF + r)
runPort t run = interpret (on _todoPort (todoPortHandler t) send) run

todoPortHandler :: forall r. TodoPortType -> TodoPort ~> Run (AFF + r)
todoPortHandler t r = case r of
  FindTodos userId next -> do
    todos <- liftAff $ t.findTodos userId
    pure $ next todos