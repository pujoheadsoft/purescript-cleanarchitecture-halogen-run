module Test.Usecases.DisplayCompletedTodosSpec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Domains.Error (Error(..))
import Domains.Todo (TodoStatus(..), TodoTitle(..), Todos, todo)
import Domains.User (UserId(..))
import Effect.Aff (Aff)
import Run (runBaseAff)
import Run.State (runState)
import Test.PMock (Param, any, fun, mock, mockFun, verify, verifyCount, (:>))
import Test.Spec (Spec, describe, it)
import Unsafe.Coerce (unsafeCoerce)
import Usecases.DisplayCompletedTodos (execute)
import Usecases.TodoOutputPort (runOutputPort)
import Usecases.TodoPort (runPort)

spec :: Spec Unit
spec = do
  describe "DisplayCompletedTodos Test" do
    it "Displays all completed todo's associated with the specified User ID" do
      let
        userId = UserId 1
        todo1 = todo (TodoTitle "Todo1") Completed
        todo2 = todo (TodoTitle "Todo2") InCompleted
        todos = [todo1, todo2]
        completedTodos = [todo1]

        findTodosFun = mockFun $ userId :> (pure $ Right todos :: Aff (Either Error Todos))
        completedTodosFun = mockFun $ todos :> completedTodos

        stateForTodosMock = mock $ completedTodos :> any

        logics = { completed: completedTodosFun }
        todoPort = { findTodos: findTodosFun }
        todoOutputPort = { 
          stateForTodos: fun stateForTodosMock,
          stateForError: unsafeCoerce
        }

      _ <- execute userId logics
        # runPort todoPort
        # runOutputPort todoOutputPort
        # runState {todos: [], errorMessage: Nothing}
        # runBaseAff
      
      verify stateForTodosMock completedTodos

    it "Display error message if an error occurs in retrieving Todo" do
      let
        userId = UserId 1

        findTodosFun = mockFun $ userId :> (pure $ Left $ Error "todo find error" :: Aff (Either Error Todos))

        stateForTodosMock = mock $ (any :: Param Todos) :> any
        stateForErrorMock = mock $ Error "todo find error" :> any

        logics = { completed: unsafeCoerce }
        todoPort = { findTodos: findTodosFun }
        todoOutputPort = {
          stateForTodos: fun stateForTodosMock,
          stateForError: fun stateForErrorMock
        }

      _ <- execute (UserId 1) logics
        # runPort todoPort
        # runOutputPort todoOutputPort
        # runState {todos: [], errorMessage: Nothing}
        # runBaseAff

      verify stateForErrorMock $ Error "todo find error"
      verifyCount stateForTodosMock 0 (any :: Param Todos)
