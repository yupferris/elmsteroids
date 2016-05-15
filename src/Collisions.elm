module Collisions exposing (collide)

import List exposing (filterMap)
import Vector exposing (..)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)

type alias CollisionProcessor a = List Bullet -> (a, List Bullet)

return : a -> CollisionProcessor a
return x = \bullets -> (x, bullets)

(>>=) : CollisionProcessor a -> (a -> CollisionProcessor b) -> CollisionProcessor b
(>>=) cp f =
  \bullets ->
    let (x, bullets') = cp bullets
    in f x bullets'

collide : List Asteroid -> List Bullet -> (List Asteroid, List Bullet)
collide asteroids =
  collide' asteroids >>= \asteroids' ->
    return (filterMap (\x -> x) asteroids')

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
