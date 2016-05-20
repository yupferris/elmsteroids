module Bullets exposing (Bullet, fire, tick, draw)

import List exposing (..)
import Collage exposing (Form, group, rect, filled, move, alpha)
import Color exposing (..)
import Vector exposing (..)
import Bounds exposing (..)
import Ship
import Player exposing (Player)

type alias Bullet =
  { position : Vector
  , velocity : Vector
  , timeUntilDeath : Float
  }

fire : Player -> List Bullet -> List Bullet
fire player bullets =
  { position = Ship.front player.position player.rotation
  , velocity =
    player.velocity
  |> add (rotate player.rotation (0, 80))
  , timeUntilDeath = 3.0
  } :: bullets

tick : Float -> List Bullet -> List Bullet
tick timeDelta = filterMap (moveBullet timeDelta >> killBullet timeDelta)

moveBullet : Float -> Bullet -> Bullet
moveBullet timeDelta bullet =
  { bullet | position =
      add bullet.position (mulS timeDelta bullet.velocity)
      |> wrap bounds }

killBullet : Float -> Bullet -> Maybe Bullet
killBullet timeDelta bullet =
  let timeUntilDeath = bullet.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then
      Just { bullet | timeUntilDeath = timeUntilDeath }
    else Nothing

draw : List Bullet -> Form
draw = map drawBullet >> group

drawBullet : Bullet -> Form
drawBullet bullet =
  rect 2 2
  |> filled white
  |> move bullet.position
  |> alpha (min bullet.timeUntilDeath 1)
