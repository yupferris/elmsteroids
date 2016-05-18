module Collisions exposing (collide)

import List exposing (map, concat, unzip)
import Random exposing (..)
import State exposing (..)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle)

collide : List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int)
collide asteroids bullets =
  collide' asteroids bullets >>= \(asteroids, bullets', particles, score) ->
    return (concat asteroids, bullets', concat particles, score)

collide' : List Asteroid -> List Bullet -> State Seed (List (List Asteroid), List Bullet, List (List SegmentParticle), Int)
collide' asteroids bullets =
  case asteroids of
    [] -> return ([], bullets, [], 0)
    x::xs ->
      testAsteroid x bullets >>= \(asteroids', bullets', particles, score) ->
        collide' xs bullets' >>= \(xs', bullets'', particles', score') ->
          return (asteroids' :: xs', bullets'', particles :: particles', score + score')

testAsteroid : Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int)
testAsteroid asteroid bullets =
  case bullets of
    [] -> return ([asteroid], [], [], 0)
    x::xs ->
      if liesInside x.position asteroid then
        split asteroid >>= \(asteroids, particles) ->
          return (asteroids, xs, particles, 100)
      else
        testAsteroid asteroid xs >>= \(asteroids, xs', particles, score) ->
          return (asteroids, x :: xs', particles, score)
