module App (
  CanvasApp,
  Impl(..),
  defaultImpl,
  makeApp,
  CanvApp(..),
  app,
  class CanvasAppSpec,
  tick,
  handleKeyboard,
  handleMouse,
  render
)
where

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
  tick :: Interval -> state -> state
  handleKeyboard :: KeyData -> state -> state
  handleMouse :: MouseData -> state -> state
  render :: state -> Effect Unit

type Impl state = {
  tick :: Interval -> state -> state,
  handleKeyboard :: KeyData -> state -> state,
  handleMouse :: MouseData -> state -> state,
  render :: state -> Effect Unit
}

defaultImpl :: forall state. Impl state
defaultImpl = {
  tick : const identity,
  handleKeyboard : const identity,
  handleMouse : const identity,
  render : const (pure unit)
}

data CanvApp state = CanvApp state (Impl state)

makeApp :: forall state. state -> Impl state -> CanvApp state
makeApp = CanvApp

instance canvasAppSpecCanvApp :: CanvasAppSpec (CanvApp state) where
  tick interval (CanvApp state impl) = CanvApp (impl.tick interval state) impl
  handleKeyboard keyData (CanvApp state impl) = CanvApp (impl.handleKeyboard keyData state) impl
  handleMouse mouseData (CanvApp state impl) = CanvApp (impl.handleMouse mouseData state) impl
  render (CanvApp state impl) = impl.render state

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
  state ->
  H.Component HH.HTML (Const Void) Unit Void Aff
app initialState =
  H.mkComponent
    { initialState: const initialState
    , render: const view
    , eval: H.mkEval $ H.defaultEval { handleAction = const (pure unit)}
    }
