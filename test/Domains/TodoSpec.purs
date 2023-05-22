module Test.Domains.TodoSpec where

import Prelude

import Domains.Todo (TodoStatus(..), TodoTitle(..), todo, completed)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Todo Test" do
    it "Returns completed Todos" do
      let
        todo1 = todo (TodoTitle "Todo1") Completed
        todo2 = todo (TodoTitle "Todo2") InCompleted
        todo3 = todo (TodoTitle "Todo3") Completed
      completed [todo1, todo2, todo3] `shouldEqual` [todo1, todo3]
