module KeyStates exposing (KeyStates, tick, pressed, released)

import Keyboard exposing (..)

type alias KeyStates =
  { left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  , spaceTapped : Bool
  }

tick : KeyStates -> KeyStates
tick keys = { keys | spaceTapped = False }

left : KeyCode
left = 37

right : KeyCode
right = 39

up : KeyCode
up = 38

down : KeyCode
down = 40

space : KeyCode
space = 32

pressed : KeyCode -> KeyStates -> KeyStates
pressed key keys =
  -- For some reason case didn't work here
  -- The compiler thought `left` and `right` were the same patterns (?)
  if key == left then { keys | left = True }
  else if key == right then { keys | right = True }
  else if key == up then { keys | up = True }
  else if key == down then { keys | down = True }
  else if key == space then { keys | spaceTapped = True }
  else keys

released : KeyCode -> KeyStates -> KeyStates
released key keys =
  if key == left then { keys | left = False }
  else if key == right then { keys | right = False }
  else if key == up then { keys | up = False }
  else if key == down then { keys | down = False }
  else keys
