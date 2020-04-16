module App (CanvasApp, app, defaultAppSpec) where

import Prelude

import Control.Monad.State as HS
import Data.Const (Const)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Timer as Timer
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Model (Interval, KeyData, KeyEvent(..), MouseButton(..), MouseData, MouseEvent(..))
import Partial.Unsafe (unsafePartial)
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

type CanvasApp
  = H.Component HH.HTML (Const Void) Unit Void Aff

type CanvasAppSpec state
  = { initialState :: state
    , tick :: Interval -> state -> state
    , handleKeyboard :: KeyData -> state -> state
    , handleMouse :: MouseData -> state -> state
    , render :: state -> Effect Unit
    }

data Msg
  = Init
  | Tick Interval
  | Keyboard KeyData
  | Mouse MouseData
  | Render

defaultAppSpec :: forall state. state -> CanvasAppSpec state
defaultAppSpec initialState =
  { initialState: initialState
  , tick: \_ s -> s
  , handleKeyboard: \_ s -> s
  , handleMouse: \_ s -> s
  , render: const (pure unit)
  }

view :: H.ComponentHTML Msg () Aff
view =
  HH.canvas
    [ HP.id_ "render-canvas"
    , HP.width 800
    , HP.height 600
    , HP.tabIndex 0
    , HE.onMouseMove (toMouseData MouseMove >>> Mouse >>> Just)
    , HE.onMouseDown (toMouseData MouseDown >>> Mouse >>> Just)
    , HE.onMouseUp (toMouseData MouseUp >>> Mouse >>> Just)
    , HE.onKeyDown (toKeyData KeyDown >>> Keyboard >>> Just)
    , HE.onKeyUp (toKeyData KeyUp >>> Keyboard >>> Just)
    ]

toKeyData :: KeyEvent -> KE.KeyboardEvent -> KeyData
toKeyData eventType event =
  { event: eventType
  , keyCode: KE.code event
  }

toMouseData :: MouseEvent -> ME.MouseEvent -> MouseData
toMouseData eventType event =
  let
    buttonInt = ME.buttons event

    button =
      if buttonInt .&. 1 > 0 then
        LeftButton
      else
        if buttonInt .&. 2 > 0 then
          RightButton
        else
          None
  in
    { event: eventType
    , button: button
    , location: { x: ME.clientX event, y: ME.clientY event }
    }

focusElement :: String -> Effect Unit
focusElement elementId = do
  document <- H.liftEffect $ Web.document =<< Web.window
  element <- H.liftEffect $ NEPN.getElementById elementId $ HTMLDocument.toNonElementParentNode document
  let
    element' = unsafePartial fromJust (element >>= HTMLElement.fromElement)
  HTMLElement.focus element'

update ::
  forall state.
  CanvasAppSpec state ->
  Msg ->
  H.HalogenM state Msg () Void Aff Unit
update appSpec = case _ of
  Init -> do
    let eventSource = ES.effectEventSource $ \emitter -> do
            let passTick = ES.emit emitter (Tick { milliseconds : 1000 })
            intervalId <- Timer.setInterval 1000 passTick
            pure $ ES.Finalizer (Timer.clearInterval intervalId)
    _ <- H.subscribe $ eventSource
    H.liftEffect $ focusElement "render-canvas"
  Tick interval -> HS.modify_ (appSpec.tick interval)
  Keyboard kbData -> HS.modify_ (appSpec.handleKeyboard kbData)
  Mouse mouseData -> HS.modify_ (appSpec.handleMouse mouseData)
  Render -> do
    currentState <- HS.get
    H.liftEffect (appSpec.render currentState)

app ::
  forall state.
  CanvasAppSpec state ->
  H.Component HH.HTML (Const Void) Unit Void Aff
app appSpec =
  H.mkComponent
    { initialState: const appSpec.initialState
    , render: const view
    , eval: H.mkEval $ H.defaultEval { handleAction = update appSpec, initialize = Just Init }
    }
