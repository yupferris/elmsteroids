module Collisions exposing (collide)

import List exposing (filterMap)
import State exposing (..)
import Vector exposing (..)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)

collide : List Asteroid -> List Bullet -> (List Asteroid, List Bullet)
collide asteroids =
  collide' asteroids >>= \asteroids ->
    return (filterMap (\x -> x) asteroids)

collide' : List Asteroid -> List Bullet -> (List (Maybe Asteroid), List Bullet)
collide' asteroids =
  case asteroids of
    [] -> return []
    x::xs ->
      testAsteroid x >>= \x' ->
        collide' xs >>= \xs' ->
          return (x' :: xs')

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
