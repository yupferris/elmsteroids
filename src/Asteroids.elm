module Asteroids exposing (Asteroid, init, tick, draw)

import List exposing (..)
import Collage exposing (group, rect, filled, move)
import Color exposing (..)
import Random exposing (float, step)
import Vector exposing (..)
import Bounds exposing (..)

type alias Asteroid =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

init seed =
  let count = 3 -- TODO: Randomize
  in init' count [] seed

init' count acc seed =
  if length acc == count then (acc, seed)
  else
    let (asteroid, seed') = initAsteroid seed
    in init' count (asteroid :: acc) seed'

initAsteroid seed =
  let
    (x, seed') = step (float left right) seed
    (y, seed'') = step (float bottom top) seed'
  in
    ({ position = (x, y)
     , velocity = (0, 0) -- TODO
     , rotation = (0, 0) -- TODO
     }, seed'')

tick timeDelta asteroids =
  asteroids

draw = map drawAsteroid >> group

drawAsteroid asteroid =
  rect 10 10
  |> filled red
  |> move asteroid.position
