module Component.Router where

import Prelude

import Control.Monad.Free (liftF)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Domains.Todo (logics)
import Domains.User (UserId(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Gateways.TodoGateway (createTodoPort)
import Halogen as H
import Halogen.HTML as HH
import Presenters.TodoPresenter (createOutputPort)
import Run (AFF, Run, match, run)
import Run.State (STATE, State(..))
import State.State (TodoState)
import Type.Row (type (+))
import Usecases.DisplayCompletedTodos (execute)
import Usecases.TodoOutputPort (runOutputPort)
import Usecases.TodoPort (runPort)

data Action = Initialize

component :: forall q i m. MonadAff m => H.Component q i Void m
component =
  H.mkComponent
    { initialState: \_ -> { todos: [], errorMessage: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { 
        initialize = Just Initialize,
        handleAction = _handleAction
      }
    }
  where
  render :: forall cs. TodoState -> H.ComponentHTML Action cs m
  render { todos } = 
    HH.ul_ $ todos <#> \todo -> HH.li_ $ [HH.text todo.title]
  
  _handleAction :: Action -> H.HalogenM TodoState Action () Void m Unit
  _handleAction = case _ of
    Initialize -> execute (UserId 1) logics
      # runPort createTodoPort
      # runOutputPort createOutputPort
      # runAffState

handleState ::
  forall state action slots output m
   . Functor m 
  => State state
  ~> H.HalogenM state action slots output m
handleState (State ss sa) = H.HalogenM $ liftF $ H.State $ Tuple <$> sa <*> ss

runAffState ::
  forall state action slots output m
  . Functor m 
  => MonadAff m 
  => Run (STATE state + AFF + ())
  ~> H.HalogenM state action slots output m
runAffState = run $ match { aff: \a -> liftAff a, state: \a -> handleState a }
