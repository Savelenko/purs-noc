module Model where

type Interval
  = { milliseconds :: Int }

data KeyEvent
  = KeyUp
  | KeyDown

type KeyData
  = { event :: KeyEvent, keyCode :: Int }

data MouseEvent
  = MouseMove
  | MouseDown
  | MouseUp

data MouseButton
  = LeftButton
  | RightButton

type Point
  = { x :: Int, y :: Int }

type MouseData
  = { event :: MouseEvent, button :: MouseButton, location :: Point }
