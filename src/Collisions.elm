module Collisions exposing (collide)

import List exposing (concat)
import State exposing (..)
import Asteroids exposing (Asteroid, liesInside)
import Bullets exposing (Bullet)

collide : List Asteroid -> List Bullet -> (List Asteroid, List Bullet)
collide asteroids =
  concat <$> collide' asteroids

collide' : List Asteroid -> List Bullet -> (List (List Asteroid), List Bullet)
collide' asteroids =
  case asteroids of
    [] -> return []
    x::xs -> (::) <$> testAsteroid x <*> collide' xs

testAsteroid : Asteroid -> List Bullet -> (List Asteroid, List Bullet)
testAsteroid asteroid bullets =
  case bullets of
    [] -> ([asteroid], [])
    x::xs ->
      if liesInside x.position asteroid then ([], xs)
      else
        let (asteroid', xs') = testAsteroid asteroid xs
        in (asteroid', x :: xs')
