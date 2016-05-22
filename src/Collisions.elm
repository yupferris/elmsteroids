module Collisions exposing (collide)

import List exposing (map, concat, concatMap, any)
import Random exposing (Seed)
import State exposing (State, finally, andThen)
import Segment exposing (..)
import Triangle
import Player exposing (Player)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle, segmentParticles)
import Ship

collide : Maybe Player -> List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int, Bool)
collide player asteroids bullets =
  collideAsteroidsBullets asteroids bullets `andThen` \(asteroids', bullets', particles, score) ->
    case player of
      Just player' ->
           collidePlayerAsteroids player' asteroids' `andThen` \(hitPlayer, particles') ->
             finally (asteroids', bullets', particles ++ particles', score, hitPlayer)
      _ -> finally (asteroids', bullets', particles, score, False)

collideAsteroidsBullets : List Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int)
collideAsteroidsBullets asteroids bullets =
  collideAsteroidsBullets' asteroids bullets `andThen` \(asteroids, bullets', particles, score) ->
    finally (concat asteroids, bullets', concat particles, score)

collideAsteroidsBullets' : List Asteroid -> List Bullet -> State Seed (List (List Asteroid), List Bullet, List (List SegmentParticle), Int)
collideAsteroidsBullets' asteroids bullets =
  case asteroids of
    [] -> finally ([], bullets, [], 0)
    x::xs ->
      collideAsteroidBullet x bullets `andThen` \(asteroids', bullets', particles, score) ->
        collideAsteroidsBullets' xs bullets' `andThen` \(xs', bullets'', particles', score') ->
          finally (asteroids' :: xs', bullets'', particles :: particles', score + score')

collideAsteroidBullet : Asteroid -> List Bullet -> State Seed (List Asteroid, List Bullet, List SegmentParticle, Int)
collideAsteroidBullet asteroid bullets =
  case bullets of
    [] -> finally ([asteroid], [], [], 0)
    x::xs ->
      if liesInside x.position asteroid then
        split asteroid `andThen` \(asteroids, particles) ->
          finally (asteroids, xs, particles, 100)
      else
        collideAsteroidBullet asteroid xs `andThen` \(asteroids, xs', particles, score) ->
          finally (asteroids, x :: xs', particles, score)

collidePlayerAsteroids : Player -> List Asteroid -> State Seed (Bool, List SegmentParticle)
collidePlayerAsteroids player asteroids =
  case asteroids of
    [] -> finally (False, [])
    x::xs ->
      collidePlayerAsteroid player x `andThen` \(hitPlayer, particles) ->
        if hitPlayer then
          finally (True, particles)
        else
          collidePlayerAsteroids player xs

collidePlayerAsteroid : Player -> Asteroid -> State Seed (Bool, List SegmentParticle)
collidePlayerAsteroid player asteroid =
  let
    shipTriangles = Ship.triangle player.position player.rotation |> Triangle.wrap
    shipSegments = concatMap Triangle.segments shipTriangles
    asteroidSegments = Asteroids.wrappedSegments asteroid
    segmentPairs = pairs shipSegments asteroidSegments
    liesInside' x = Asteroids.liesInside x asteroid
  in
    if
      any (\(x, y) -> intersect x y) segmentPairs
      || any (\t -> liesInside' t.a || liesInside' t.b || liesInside' t.c) shipTriangles then
      segmentParticles player.velocity shipSegments `andThen` \particles ->
        finally (True, particles)
    else finally (False, [])

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
