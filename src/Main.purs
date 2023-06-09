module Main where

import Prelude

import Component.Router as Router
import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Router.component unit body

-- main :: Effect Unit
-- main =
--   launchAff_ do
--     let
--       todoLogics = logics
--     execute (UserId 1) todoLogics
--       # runPort createTodoPort
--       # runOutputPort createOutputPort
--       # runBaseAff
