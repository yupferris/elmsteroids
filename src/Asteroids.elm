module Asteroids exposing (Asteroid, liesInside, split, init, tick, draw)

import List exposing (map, any)
import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color exposing (..)
import DrawWrapped exposing (..)
import Random exposing (Seed, int, float, step)
import State exposing (..)
import Vector exposing (..)
import Segment exposing (..)
import Triangle exposing (..)
import Bounds
import SegmentParticles exposing (SegmentParticle, segmentParticle)

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


split : Asteroid -> State Seed (List Asteroid, List SegmentParticle)
split asteroid =
  segmentParticles asteroid.velocity (segments asteroid) >>= \particles ->
    let size = asteroid.size - 1
    in
      if size > 0 then
        let
          (left, bottom) = asteroid.position
          (right, top) = asteroid.position
          initAsteroid' = initAsteroid left right bottom top Nothing size size
        in
          initAsteroid' >>= \a ->
            initAsteroid' >>= \b ->
              return ([a, b], particles)
      else return ([], particles)

segmentParticles : Vector -> List Segment -> State Seed (List SegmentParticle)
segmentParticles initialVelocity segments =
  case segments of
    [] -> return []
    x::xs ->
      (::) <$> segmentParticle initialVelocity x <*> segmentParticles initialVelocity xs

init : State Seed (List Asteroid)
init = step (int 2 3) >>= init' 4 5

init' : Int -> Int -> Int -> State Seed (List Asteroid)
init' minSize maxSize count =
  if count == 0 then return []
  else
    (::)
      <$> initAsteroid Bounds.left Bounds.right Bounds.bottom Bounds.top (Just Bounds.safeZoneSize) minSize maxSize
      <*> init' minSize maxSize (count - 1)

-- TODO: Consider proper bounds type
initAsteroid : Float -> Float -> Float -> Float -> Maybe Float -> Int -> Int -> State Seed Asteroid
initAsteroid left right bottom top safeZoneSize minSize maxSize =
  let
    angle = float 0 (pi * 2) |> step
    radiusGen size =
      let
        idealRadius = toFloat size * 16.0
        minRadius = idealRadius * 0.95
        maxRadius = idealRadius * 1.05
      in float minRadius maxRadius
  in
    step (float left right) >>= \x ->
      step (float bottom top) >>= \y ->
        step (int minSize maxSize) >>= \size ->
          angle >>= \velDirection ->
            step (float 40 (100 / toFloat (size ^ 4))) >>= \velMagnitude ->
              angle >>= \rotation ->
                step (float -0.5 0.5) >>= \rotationVelocity ->
                  step (radiusGen size) >>= \radius ->
                    let
                      minRadius = radius * 0.8
                      maxRadius = radius * 1.2

                      position =
                        let
                          p = (x, y)
                        in
                          case safeZoneSize of
                            Just s ->
                                 let safeZoneSize' = s + radius
                                 in
                                   if length p < safeZoneSize' then
                                     p |> normalize |> mul safeZoneSize'
                                   else p
                            _ -> p
                    in
                      initPoints minRadius maxRadius >>= \points ->
                        return
                          { position = position
                          , velocity = rotate velDirection (0, velMagnitude)
                          , rotation = rotation
                          , rotationVelocity = rotationVelocity
                          , size = size
                          , points = points
                          }

initPoints : Float -> Float -> State Seed (List Vector)
initPoints minRadius maxRadius =
  step (int 10 16) >>= \count ->
    initPoints' count (pi * 2.0 / (toFloat count)) minRadius maxRadius

initPoints' : Int -> Float -> Float -> Float -> State Seed (List Vector)
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
      |> wrap Bounds.bounds }

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
