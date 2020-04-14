module App (CanvasApp, app, defaultAppSpec, class CanvasAppSpec, tick, handleKeyboard, handleMouse,  render) where

import Prelude
import Control.Monad.State as HS
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Model (Interval, KeyData, MouseData)

type CanvasApp
  = H.Component HH.HTML (Const Void) Unit Void Aff


data Msg
  = Tick Interval
  | Keyboard KeyData
  | Mouse MouseData
  | Render


class CanvasAppSpec state where
    initialState :: state
    tick :: Interval -> state -> state
    handleKeyboard :: KeyData -> state -> state
    handleMouse :: MouseData -> state -> state
    render :: state -> Effect Unit

-- defaultAppSpec :: forall state. state -> CanvasAppSpec state
type defaultAppspec state

instance defaultCanvasAppSpec :: CanvasAppSpec state where
  initialState: initialState
  tick: \_ s -> s
  handleKeyboard: \_ s -> s
  handleMouse: \_ s -> s
  render: const (pure unit)

foreign import ignore :: forall a. a -> Effect Unit

view :: H.ComponentHTML Msg () Aff
view = HH.canvas [ HP.id_ "render-canvas", HP.width 800, HP.height 600 ]

update ::
  forall state. CanvasAppSpec state =>
  Msg ->
  H.HalogenM state Msg () Void Aff Unit
update = case _ of
  Tick interval -> HS.modify_ (\s -> tick interval s)
  Keyboard kbData -> HS.modify_ (\s -> handleKeyboard kbData s)
  Mouse mouseData -> HS.modify_ (\s -> handleMouse mouseData s)
  Render -> do
    currentState <- HS.get
    H.liftEffect (render currentState)

app ::
  forall state. CanvasAppSpec state =>
  H.Component HH.HTML (Const Void) Unit Void Aff
app =
  H.mkComponent
    { initialState: const initialState???
    , render: const view
    , eval: H.mkEval $ H.defaultEval { handleAction = update ??? }
    }
