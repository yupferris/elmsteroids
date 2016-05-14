module Asteroids exposing (Asteroid, init, tick, draw)

import List exposing (..)
import Collage exposing (Form, group, polygon, filled, outlined, move, defaultLine)
import Color exposing (..)
import DrawWrapped exposing (..)
import Random exposing (Seed, int, float, step)
import RandomProcessor exposing (..)
import Vector exposing (..)
import Bounds exposing (..)

type alias Asteroid =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  , rotationVelocity : Float
  , points : List Vector
  }

init : Seed -> (List Asteroid, Seed)
init =
  step (int 3 8) >>= \count ->
    init' count

init' : Int -> Seed -> (List Asteroid, Seed)
init' count =
  if count == 0 then return []
  else
    initAsteroid >>= \asteroid ->
      init' (count - 1) >>= \acc ->
        return (asteroid :: acc)

initAsteroid : Seed -> (Asteroid, Seed)
initAsteroid =
  let angle = float 0 (pi * 2) |> step
  in
    step (float left right) >>= \x ->
      step (float bottom top) >>= \y ->
        angle >>= \velDirection ->
          step (float 0 10) >>= \velMagnitude ->
            angle >>= \rotation ->
              step (float -0.5 0.5) >>= \rotationVelocity ->
                initPoints >>= \points ->
                  return
                    { position = (x, y)
                    , velocity = mul velMagnitude (cos velDirection, sin velDirection)
                    , rotation = rotation
                    , rotationVelocity = rotationVelocity
                    , points = points
                    }

initPoints : Seed -> (List Vector, Seed)
initPoints =
  step (int 6 10) >>= \count ->
    step (float 20.0 70.0) >>= \size ->
      initPoints' count (pi * 2.0 / (toFloat count)) size

initPoints' : Int -> Float -> Float -> Seed -> (List Vector, Seed)
initPoints' count segAngleDelta size =
  if count == 0 then return []
  else
    let angleOffset = toFloat count * segAngleDelta
    in
      step (float (-segAngleDelta * 0.3) (segAngleDelta * 0.3)) >>= \angle ->
        let
          angle' = angle + angleOffset
          x = cos angle' * size
          y = sin angle' * size
          point = (x, y)
        in
          initPoints' (count - 1) segAngleDelta size >>= \acc ->
            return (point :: acc)

tick : Float -> List Asteroid -> List Asteroid
tick timeDelta = map (moveAsteroid timeDelta >> rotateAsteroid timeDelta)

moveAsteroid : Float -> Asteroid -> Asteroid
moveAsteroid timeDelta asteroid =
  { asteroid | position =
      add asteroid.position (mul timeDelta asteroid.velocity)
      |> wrap bounds }

rotateAsteroid : Float -> Asteroid -> Asteroid
rotateAsteroid timeDelta asteroid =
  { asteroid | rotation =
      asteroid.rotation + asteroid.rotationVelocity * timeDelta }

draw : List Asteroid -> Form
draw = map drawAsteroid >> group

drawAsteroid : Asteroid -> Form
drawAsteroid asteroid =
  let
    shape =
      asteroid.points
      |> polygon
  in
    group
      [ shape |> filled black
      , shape |> outlined { defaultLine | color = white }
      ]
    |> Collage.rotate asteroid.rotation
    |> move asteroid.position
    |> drawWrapped
