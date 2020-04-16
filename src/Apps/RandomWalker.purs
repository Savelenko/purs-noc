module Apps.RandomWalker (app) where

import Prelude

import App as App
import Debug as Debug
import Effect.Console (log)


app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec "")
        { render = \state -> log state
        , handleKeyboard = \e s -> Debug.unsafeLog e.keyCode
        , handleMouse = \e s -> Debug.unsafeLog e.location # \_ -> Debug.unsafeLog e # \_ -> s
        , tick = \_ s -> Debug.unsafeLog "Tick!" # \_ -> s
        }
