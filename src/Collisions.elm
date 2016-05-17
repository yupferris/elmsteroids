module Collisions exposing (collide)

import List exposing (concat)
import Random exposing (..)
import State exposing (..)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)

collide : List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet)
collide asteroids bullets =
  collide' asteroids bullets >>= \(asteroids', bullets') ->
    return (concat asteroids', bullets')

collide' : List Asteroid -> List Bullet -> State Seed (List (List Asteroid), List Bullet)
collide' asteroids bullets =
  case asteroids of
    [] -> return ([], bullets)
    x::xs ->
      testAsteroid x bullets >>= \(x', bullets') ->
        collide' xs bullets' >>= \(xs', bullets'') ->
          return (x' :: xs', bullets'')

testAsteroid : Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet)
testAsteroid asteroid bullets =
  case bullets of
    [] -> return ([asteroid], [])
    x::xs ->
      if liesInside x.position asteroid then
        split asteroid >>= \asteroids ->
          return (asteroids, xs)
      else
        testAsteroid asteroid xs >>= \(asteroids, xs') ->
          return (asteroids, x :: xs')
