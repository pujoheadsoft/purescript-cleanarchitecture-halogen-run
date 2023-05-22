module Gateways.TodoGateway where

import Prelude

import Affjax (printError)
import Affjax.Node (get)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Domains.Error (Error(..))
import Domains.Todo (TodoStatus(..), TodoTitle(..), Todos, todo)
import Domains.User (UserId(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Simple.JSON (readJSON)
import Usecases.TodoPort (TodoPortType)

type TodoJson = {
  title :: String,
  completed :: Boolean
}

type TodosJson = Array TodoJson

createTodoPort :: TodoPortType
createTodoPort = { findTodos: findTodos }

findTodos :: forall m. MonadAff m => UserId -> m (Either Error Todos)
findTodos (UserId id) = do
  res <- liftAff $ get string $ "https://jsonplaceholder.typicode.com/users/" <> show id <> "/todos"
  case res of
    Left err -> do
      pure $ Left $ Error $ "GET /api response failed to decode: " <> printError err
    Right response -> do
      case readJSON response.body of
        Right (todos :: TodosJson) -> do
          pure $ Right $ todos <#> (\{title, completed} -> todo (TodoTitle title) if completed then Completed else InCompleted)
        Left e -> do
          pure $ Left $ Error $ "Can't parse JSON. " <> show e
