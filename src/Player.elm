module Player exposing (Player, tick, draw)

import Collage exposing (Form)
import Color exposing (..)
import DrawWrapped exposing (..)
import Vector exposing (..)
import Bounds exposing (..)
import Ship
import KeyStates exposing (KeyStates)

type alias Player =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

tick : Float -> KeyStates -> Player -> Player
tick timeDelta keys player =
  let
    position =
      add player.position (mulS timeDelta player.velocity)
      |> wrap bounds

    accel = 57.0
    upAccel = if keys.up then accel else 0
    downAccel = if keys.down then -accel else 0
    velocityDelta = upAccel + downAccel
    velocity =
      (0, velocityDelta * timeDelta)
      |> rotate player.rotation
      |> add player.velocity

    rotationSpeed = 1.5
    leftDelta = if keys.left then -rotationSpeed else 0
    rightDelta = if keys.right then rotationSpeed else 0
    rotationDelta = leftDelta + rightDelta
    rotation = player.rotation + rotationDelta * timeDelta

  in
    { player
      | position = position
      , velocity = velocity
      , rotation = rotation
    }

draw : Player -> Form
draw player =
  Ship.draw player.position player.rotation
  |> drawWrapped
