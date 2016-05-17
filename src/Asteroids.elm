module Asteroids exposing (Asteroid, liesInside, split, init, tick, draw)

import List exposing (map, any)
import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color exposing (..)
import DrawWrapped exposing (..)
import Random exposing (Seed, int, float, step)
import State exposing (..)
import Vector exposing (..)
import Triangle exposing (Triangle)
import Segment exposing (Segment, center)
import Bounds exposing (..)

type alias Asteroid =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  , rotationVelocity : Float
  , size : Int
  , points : List Vector
  }

absolutePoints : Asteroid -> List Vector
absolutePoints asteroid =
  asteroid.points
    |> map (rotate asteroid.rotation >> add asteroid.position)

liesInside : Vector -> Asteroid -> Bool
liesInside point =
  -- TODO: Handle wrapping
  -- Currently it's possible to shoot through parts of an asteroid
  -- that are wrapping around the screen. This will also be a
  -- problem when checking for ship/asteroid collision.
  triangles >> any (Triangle.liesInside point)

triangles : Asteroid -> List Triangle
triangles asteroid =
  asteroid
    |> segments
    |> map
       (\segment ->
          { a = segment.a
          , b = segment.b
          , c = asteroid.position
          })

segments : Asteroid -> List Segment
segments asteroid =
  let points = absolutePoints asteroid
  in
    case points of
      [] -> []
      x::_ -> segments' x points

segments' : Vector -> List Vector -> List Segment
segments' firstPoint points =
  case points of
    [] -> []
    x::xs ->
      let
        next =
          case xs of
            [] -> firstPoint
            y::_ -> y
        segment =
          { a = x
          , b = next
          }
      in segment :: segments' firstPoint xs


split : Asteroid -> Seed -> (List Asteroid, Seed)
split asteroid =
  let size = asteroid.size - 1
  in
    if size > 0 then
      let
        (boundsLeft, boundsBottom) = asteroid.position
        (boundsRight, boundsTop) = asteroid.position
        initAsteroid' = initAsteroid boundsLeft boundsRight boundsBottom boundsTop size size
      in
        initAsteroid' >>= \a ->
          initAsteroid' >>= \b ->
            return [a, b]
    else return []

init : Seed -> (List Asteroid, Seed)
init = step (int 2 3) >>= init' 4 5

init' : Int -> Int -> Int -> Seed -> (List Asteroid, Seed)
init' minSize maxSize count =
  if count == 0 then return []
  else
    (::)
      <$> initAsteroid left right bottom top minSize maxSize
      <*> init' minSize maxSize (count - 1)

-- TODO: Consider proper bounds type
initAsteroid : Float -> Float -> Float -> Float -> Int -> Int -> Seed -> (Asteroid, Seed)
initAsteroid boundsLeft boundsRight boundsBottom boundsTop minSize maxSize =
  let
    angle = float 0 (pi * 2) |> step
    radiusGen size =
      let
        idealRadius = toFloat size * 16.0
        minRadius = idealRadius * 0.95
        maxRadius = idealRadius * 1.05
      in float minRadius maxRadius
  in
    step (float boundsLeft boundsRight) >>= \x ->
      step (float boundsBottom boundsTop) >>= \y ->
        step (int minSize maxSize) >>= \size ->
          angle >>= \velDirection ->
            step (float 40 (100 / toFloat (size ^ 4))) >>= \velMagnitude ->
              angle >>= \rotation ->
                step (float -0.5 0.5) >>= \rotationVelocity ->
                  step (radiusGen size) >>= \radius ->
                    let
                      minRadius = radius * 0.8
                      maxRadius = radius * 1.2

                      safeZoneSize' = safeZoneSize + radius
                      position = (x, y)
                      position' =
                        if length position < safeZoneSize' then
                          position |> normalize |> mul safeZoneSize'
                        else position
                    in
                      initPoints minRadius maxRadius >>= \points ->
                        return
                          { position = position'
                          , velocity = mul velMagnitude (cos velDirection, sin velDirection)
                          , rotation = rotation
                          , rotationVelocity = rotationVelocity
                          , size = size
                          , points = points
                          }

initPoints : Float -> Float -> Seed -> (List Vector, Seed)
initPoints minRadius maxRadius =
  step (int 10 16) >>= \count ->
    initPoints' count (pi * 2.0 / (toFloat count)) minRadius maxRadius

initPoints' : Int -> Float -> Float -> Float -> Seed -> (List Vector, Seed)
initPoints' count segAngleDelta minRadius maxRadius =
  if count == 0 then return []
  else
    let angleOffset = toFloat count * segAngleDelta
    in
      step (float (-segAngleDelta * 0.3) (segAngleDelta * 0.3)) >>= \angle ->
        step (float minRadius maxRadius) >>= \radius' ->
          let
            angle' = angle + angleOffset
            x = cos angle' * radius'
            y = sin angle' * radius'
            point = (x, y)
          in
            initPoints' (count - 1) segAngleDelta minRadius maxRadius >>= \acc ->
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
      asteroid
        |> absolutePoints
        |> polygon
  in
    group
      [ shape |> filled black
      , shape |> outlined { defaultLine | color = white }
      ]
    |> drawWrapped
