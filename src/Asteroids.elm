module Asteroids exposing (Asteroid, init, tick, draw)

import List exposing (..)
import Collage exposing (group, rect, filled, move)
import Color exposing (..)
import Random exposing (int, float, step)
import Vector exposing (..)
import Bounds exposing (..)

type alias Asteroid =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

init seed =
  let (count, seed') = step (int 1 5) seed
  in init' count [] seed'

init' count acc seed =
  if length acc == count then (acc, seed)
  else
    let (asteroid, seed') = initAsteroid seed
    in init' count (asteroid :: acc) seed'

initAsteroid seed =
  let
    (x, seed') = step (float left right) seed
    (y, seed'') = step (float bottom top) seed'
    (velDirection, seed''') = step (float 0 (pi * 2)) seed''
    (velMagnitude, seed'''') = step (float 0 10) seed'''
  in
    ({ position = (x, y)
     , velocity = mul velMagnitude (cos velDirection, sin velDirection)
     , rotation = 0 -- TODO
     }, seed'''')

tick timeDelta = map (moveAsteroid timeDelta)

moveAsteroid timeDelta asteroid =
  { asteroid | position =
      add asteroid.position (mul timeDelta asteroid.velocity)
      |> wrap bounds }

draw = map drawAsteroid >> group

drawAsteroid asteroid =
  rect 10 10
  |> filled red
  |> move asteroid.position
