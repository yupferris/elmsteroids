module Player exposing (Player, tick, draw)

import Collage exposing (group)
import Color exposing (..)
import Vector exposing (..)
import Bounds exposing (..)
import Ship

type alias Player =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

tick timeDelta keys player =
  let
    position =
      add player.position (mul timeDelta player.velocity)
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

draw player =
  let
    position = player.position
    rotation = player.rotation

    drawShip offset = Ship.draw (add position offset) rotation
  in
    group
      -- TODO: Gonna probably need to generalize this wrapping logic
      [ Ship.draw position rotation
      , drawShip (-width, 0)
      , drawShip (width, 0)
      , drawShip (0, -height)
      , drawShip (0, height)
      , drawShip (-width, -height)
      , drawShip (width, -height)
      , drawShip (-width, height)
      , drawShip (width, height)
      ]
