module KeyStates exposing (KeyStates, tick, pressed, released)

type alias KeyStates =
  { left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  , spaceTapped : Bool
  }

tick keys = { keys | spaceTapped = False }

left = 37
right = 39
up = 38
down = 40
space = 32

pressed key keys =
  -- For some reason case didn't work here
  -- The compiler thought `left` and `right` were the same patterns (?)
  if key == left then { keys | left = True }
  else if key == right then { keys | right = True }
  else if key == up then { keys | up = True }
  else if key == down then { keys | down = True }
  else if key == space then { keys | spaceTapped = True }
  else keys

released key keys =
  if key == left then { keys | left = False }
  else if key == right then { keys | right = False }
  else if key == up then { keys | up = False }
  else if key == down then { keys | down = False }
  else keys
