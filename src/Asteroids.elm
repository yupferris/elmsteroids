module Asteroids exposing (Asteroid, init, tick, draw)

import List exposing (..)
import Collage exposing (group, rect, filled, move)
import Color exposing (..)
import Random exposing (Seed, int, float, step)
import RandomProcessor exposing (..)
import Vector exposing (..)
import Bounds exposing (..)

type alias Asteroid =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

init =
  step (int 1 5) >>= \count ->
    init' count []

init' count acc =
  if length acc == count then return acc
  else
    initAsteroid >>= \asteroid ->
      init' count (asteroid :: acc)

initAsteroid =
  let angleGen = float 0 (pi * 2)
  in
    step (float left right) >>= \x ->
      step (float bottom top) >>= \y ->
        step angleGen >>= \velDirection ->
          step (float 0 10) >>= \velMagnitude ->
            step angleGen >>= \rotation ->
              return
                { position = (x, y)
                , velocity = mul velMagnitude (cos velDirection, sin velDirection)
                , rotation = rotation
                }

tick timeDelta = map (moveAsteroid timeDelta)

moveAsteroid timeDelta asteroid =
  { asteroid | position =
      add asteroid.position (mul timeDelta asteroid.velocity)
      |> wrap bounds }

draw = map drawAsteroid >> group

drawAsteroid asteroid =
  rect 10 10
  |> filled red
  |> Collage.rotate asteroid.rotation
  |> move asteroid.position
