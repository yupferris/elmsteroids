module Bullets exposing (Bullet, tick, draw)

import List exposing (..)
import Collage exposing (group, rect, filled, move, alpha)
import Color exposing (..)
import Vector exposing (..)
import Bounds exposing (..)
import Ship

type alias Bullet =
  { position : Vector
  , velocity : Vector
  , timeUntilDeath : Float
  }

tick timeDelta keys player =
  fireBullet keys player
  >> filterMap (moveBullet timeDelta >> killBullet timeDelta)

fireBullet keys player bullets =
  if keys.spaceTapped then
    { position = Ship.front player.position player.rotation
    , velocity =
      player.velocity
      |> add (rotate player.rotation (0, 60))
    , timeUntilDeath = 5.0
    } :: bullets
  else
    bullets

moveBullet timeDelta bullet =
  { bullet | position =
      add bullet.position (mul timeDelta bullet.velocity)
      |> wrap bounds }

killBullet timeDelta bullet =
  let timeUntilDeath = bullet.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then
      Just { bullet | timeUntilDeath = timeUntilDeath }
    else Nothing

draw = map drawBullet >> group

drawBullet bullet =
  rect 2 2
  |> filled white
  |> move bullet.position
  |> alpha (min bullet.timeUntilDeath 1)
