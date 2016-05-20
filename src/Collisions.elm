module Collisions exposing (collide)

import List exposing (map, concat, unzip)
import Random exposing (..)
import State exposing (..)
import Player exposing (Player)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle)

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
  return (False, [])
