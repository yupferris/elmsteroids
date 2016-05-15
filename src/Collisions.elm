module Collisions exposing (collide)

import Vector exposing (..)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)

collide : List Asteroid -> List Bullet -> (List Asteroid, List Bullet)
collide asteroids bullets =
  case asteroids of
    [] -> ([], bullets)
    x::xs ->
      let
        (x', bullets') = testAsteroid x bullets
        (xs', bullets'') = collide xs bullets'
      in
        let xs'' =
          case x' of
            Just x'' -> x'' :: xs'
            _ -> xs'
        in (xs'', bullets'')

testAsteroid : Asteroid -> List Bullet -> (Maybe Asteroid, List Bullet)
testAsteroid asteroid bullets =
  case bullets of
    [] -> (Just asteroid, [])
    x::xs ->
      let
        distance = length (sub asteroid.position x.position)
        collides = distance <= asteroid.radius
      in
        if collides then (Nothing, xs)
        else
          let (asteroid', xs') = testAsteroid asteroid xs
          in (asteroid', x :: xs')
