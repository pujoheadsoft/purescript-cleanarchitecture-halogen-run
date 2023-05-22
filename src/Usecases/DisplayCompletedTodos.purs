module Usecases.DisplayCompletedTodos where

import Prelude

import Data.Either (Either(..))
import Domains.Todo (Logics)
import Domains.User (UserId)
import Run (Run, AFF)
import Run.State (STATE)
import State.State (TodoState)
import Type.Row (type (+))
import Usecases.TodoOutputPort (TODO_OUTPUT_PORT, setTodos, setError)
import Usecases.TodoPort (TODO_PORT, findTodos)

{-
  Run (AFF + ...) Unit
  か
  MonadAff m => Run (...) (m Unit)
  のどちらかでないと駄目っぽい

  なぜなら
  MonadState State m => Run (AFF + ...) (m Unit)
  だと
  runBaseAffなどしたときにどうしても
  Aff (m Unit)
  になったり
  MoadAff m => MonadState State m => m (m Unit)
  になってしまうから

  AFFのパターンでいくなら、MonadStateのmを返せないので、どこかでMonadStateに変換できないといけない。
  RunのSTATE使って、やれるか？

  MonadAffのパターンのほうがまだできそうか？

-}

execute 
  :: UserId
  -> Logics
  -> Run (TODO_PORT + TODO_OUTPUT_PORT + AFF + STATE TodoState + ()) Unit
execute id logics = do
  result <- findTodos id
  case result of
    Right todos -> setTodos $ logics.completed todos
    Left e -> setError e
