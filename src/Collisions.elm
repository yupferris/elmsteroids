module Collisions exposing (collide)

import List exposing (map, concat, unzip)
import Random exposing (..)
import State exposing (..)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle)

collide : List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle)
collide asteroids bullets =
  collide' asteroids bullets >>= \(asteroidsAndParticles, bullets') ->
    let (asteroids', particles) = unzip asteroidsAndParticles
    in return (concat asteroids', bullets', concat particles)

collide' : List Asteroid -> List Bullet -> State Seed (List (List Asteroid, List SegmentParticle), List Bullet)
collide' asteroids bullets =
  case asteroids of
    [] -> return ([], bullets)
    x::xs ->
      testAsteroid x bullets >>= \(asteroids', particles, bullets') ->
        collide' xs bullets' >>= \(xs', bullets'') ->
          return ((asteroids', particles) :: xs', bullets'')

testAsteroid : Asteroid -> List Bullet -> State Seed (List Asteroid, List SegmentParticle, List Bullet)
testAsteroid asteroid bullets =
  case bullets of
    [] -> return ([asteroid], [], [])
    x::xs ->
      if liesInside x.position asteroid then
        split asteroid >>= \(asteroids, particles) ->
          return (asteroids, particles, xs)
      else
        testAsteroid asteroid xs >>= \(asteroids, particles, xs') ->
          return (asteroids, particles, x :: xs')
