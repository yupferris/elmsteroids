module Collisions exposing (collide)

import List exposing (map, concat, unzip)
import Random exposing (..)
import State exposing (..)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle)

collide : List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle)
collide asteroids bullets =
  collide' asteroids bullets >>= \(asteroids, bullets', particles) ->
    return (concat asteroids, bullets', concat particles)

collide' : List Asteroid -> List Bullet -> State Seed (List (List Asteroid), List Bullet, List (List SegmentParticle))
collide' asteroids bullets =
  case asteroids of
    [] -> return ([], bullets, [])
    x::xs ->
      testAsteroid x bullets >>= \(asteroids', bullets', particles) ->
        collide' xs bullets' >>= \(xs', bullets'', particles') ->
          return (asteroids' :: xs', bullets'', particles :: particles')

testAsteroid : Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle)
testAsteroid asteroid bullets =
  case bullets of
    [] -> return ([asteroid], [], [])
    x::xs ->
      if liesInside x.position asteroid then
        split asteroid >>= \(asteroids, particles) ->
          return (asteroids, xs, particles)
      else
        testAsteroid asteroid xs >>= \(asteroids, xs', particles) ->
          return (asteroids, x :: xs', particles)
