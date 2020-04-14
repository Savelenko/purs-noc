module Apps.RandomWalker (app) where

import Effect.Console (logShow)
import App as App

app :: App.CanvasApp
app = App.app (App.makeApp { x: 0, y: 0 } App.defaultImpl { render = (\state -> logShow state.x) })
