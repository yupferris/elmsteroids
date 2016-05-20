module Bullets exposing (Bullet, tick, draw)

import List exposing (..)
import Collage exposing (Form, group, rect, filled, move, alpha)
import Color exposing (..)
import Vector exposing (..)
import Bounds exposing (..)
import Ship
import Player exposing (Player)
import KeyStates exposing (KeyStates)

type alias Bullet =
  { position : Vector
  , velocity : Vector
  , timeUntilDeath : Float
  }

tick : Float -> KeyStates -> Player -> List Bullet -> List Bullet
tick timeDelta keys player =
  fireBullet keys player
  >> filterMap (moveBullet timeDelta >> killBullet timeDelta)

fireBullet : KeyStates -> Player -> List Bullet -> List Bullet
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
