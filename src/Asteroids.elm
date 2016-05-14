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
  , rotationVelocity : Float
  }

init =
  step (int 1 5) >>= \count ->
    init' count

init' count =
  if count == 0 then return []
  else
    initAsteroid >>= \asteroid ->
      init' (count - 1) >>= \acc ->
        return (asteroid :: acc)

initAsteroid =
  let angle = float 0 (pi * 2) |> step
  in
    step (float left right) >>= \x ->
      step (float bottom top) >>= \y ->
        angle >>= \velDirection ->
          step (float 0 10) >>= \velMagnitude ->
            angle >>= \rotation ->
              step (float -0.5 0.5) >>= \rotationVelocity ->
                return
                  { position = (x, y)
                  , velocity = mul velMagnitude (cos velDirection, sin velDirection)
                  , rotation = rotation
                  , rotationVelocity = rotationVelocity
                  }

tick timeDelta = map (moveAsteroid timeDelta >> rotateAsteroid timeDelta)

moveAsteroid timeDelta asteroid =
  { asteroid | position =
      add asteroid.position (mul timeDelta asteroid.velocity)
      |> wrap bounds }

rotateAsteroid timeDelta asteroid =
  { asteroid | rotation =
      asteroid.rotation + asteroid.rotationVelocity * timeDelta }

draw = map drawAsteroid >> group

drawAsteroid asteroid =
  rect 10 10
  |> filled red
  |> Collage.rotate asteroid.rotation
  |> move asteroid.position
