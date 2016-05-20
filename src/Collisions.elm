module Collisions exposing (collide)

import List exposing (map, concat, any)
import Random exposing (Seed)
import State exposing (..)
import Segment exposing (..)
import Triangle
import Player exposing (Player)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle, segmentParticles)
import Ship

collide : Maybe Player -> List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int, Bool)
collide player asteroids bullets =
  collideAsteroidsBullets asteroids bullets >>= \(asteroids', bullets', particles, score) ->
    case player of
      Just player' ->
           collidePlayerAsteroids player' asteroids' >>= \(hitPlayer, particles') ->
             return (asteroids', bullets', particles ++ particles', score, hitPlayer)
      _ -> return (asteroids', bullets', particles, score, False)

collideAsteroidsBullets : List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int)
collideAsteroidsBullets asteroids bullets =
  collideAsteroidsBullets' asteroids bullets >>= \(asteroids, bullets', particles, score) ->
    return (concat asteroids, bullets', concat particles, score)

collideAsteroidsBullets' : List Asteroid -> List Bullet -> State Seed (List (List Asteroid), List Bullet, List (List SegmentParticle), Int)
collideAsteroidsBullets' asteroids bullets =
  case asteroids of
    [] -> return ([], bullets, [], 0)
    x::xs ->
      collideAsteroidBullet x bullets >>= \(asteroids', bullets', particles, score) ->
        collideAsteroidsBullets' xs bullets' >>= \(xs', bullets'', particles', score') ->
          return (asteroids' :: xs', bullets'', particles :: particles', score + score')

collideAsteroidBullet : Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int)
collideAsteroidBullet asteroid bullets =
  case bullets of
    [] -> return ([asteroid], [], [], 0)
    x::xs ->
      if liesInside x.position asteroid then
        split asteroid >>= \(asteroids, particles) ->
          return (asteroids, xs, particles, 100)
      else
        collideAsteroidBullet asteroid xs >>= \(asteroids, xs', particles, score) ->
          return (asteroids, x :: xs', particles, score)

collidePlayerAsteroids : Player -> List Asteroid -> State Seed (Bool, List SegmentParticle)
collidePlayerAsteroids player asteroids =
  case asteroids of
    [] -> return (False, [])
    x::xs ->
      collidePlayerAsteroid player x >>= \(hitPlayer, particles) ->
        if hitPlayer then
          return (True, particles)
        else
          collidePlayerAsteroids player xs

collidePlayerAsteroid : Player -> Asteroid -> State Seed (Bool, List SegmentParticle)
collidePlayerAsteroid player asteroid =
  let
    ship = Ship.triangle player.position player.rotation
    shipSegments = Triangle.segments ship
    asteroidSegments = Asteroids.segments asteroid
    segmentPairs = pairs shipSegments asteroidSegments
  in
    if any (\(x, y) -> intersect x y) segmentPairs || Asteroids.liesInside ship.a asteroid then
      segmentParticles player.velocity shipSegments >>= \particles ->
        return (True, particles)
    else return (False, [])

-- TODO: Probably a better way to do this
pairs : List a -> List b -> List (a, b)
pairs a b =
  case a of
    [] -> []
    x::xs -> pairs' x b ++ pairs xs b

pairs' : a -> List b -> List (a, b)
pairs' x b =
  case b of
    [] -> []
    y::ys -> (x, y) :: pairs' x ys
