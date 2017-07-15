module Collisions exposing (collide)

import List exposing (map, concat, concatMap, any)
import Random exposing (Seed)
import State exposing (..)
import Segment exposing (..)
import Triangle
import Player exposing (Player)
import Asteroids exposing (Asteroid, liesInside, split)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle, segmentParticles)
import Ship


collide : Maybe Player -> List Asteroid -> List Bullet -> State Seed ( List Asteroid, List Bullet, List SegmentParticle, Int, Bool )
collide player asteroids bullets =
    collideAsteroidsBullets asteroids bullets
        >>= \( asteroids_, bullets_, particles, score ) ->
                case player of
                    Just player_ ->
                        collidePlayerAsteroids player_ asteroids_
                            >>= \( hitPlayer, particles_ ) ->
                                    return ( asteroids_, bullets_, particles ++ particles_, score, hitPlayer )

                    _ ->
                        return ( asteroids_, bullets_, particles, score, False )


collideAsteroidsBullets : List Asteroid -> List Bullet -> State Seed ( List Asteroid, List Bullet, List SegmentParticle, Int )
collideAsteroidsBullets asteroids bullets =
    collideAsteroidsBullets_ asteroids bullets
        >>= \( asteroids, bullets_, particles, score ) ->
                return ( concat asteroids, bullets_, concat particles, score )


collideAsteroidsBullets_ : List Asteroid -> List Bullet -> State Seed ( List (List Asteroid), List Bullet, List (List SegmentParticle), Int )
collideAsteroidsBullets_ asteroids bullets =
    case asteroids of
        [] ->
            return ( [], bullets, [], 0 )

        x :: xs ->
            collideAsteroidBullet x bullets
                >>= \( asteroids_, bullets_, particles, score ) ->
                        collideAsteroidsBullets_ xs bullets_
                            >>= \( xs_, bullets__, particles_, score_ ) ->
                                    return ( asteroids_ :: xs_, bullets__, particles :: particles_, score + score_ )


collideAsteroidBullet : Asteroid -> List Bullet -> State Seed ( List Asteroid, List Bullet, List SegmentParticle, Int )
collideAsteroidBullet asteroid bullets =
    case bullets of
        [] ->
            return ( [ asteroid ], [], [], 0 )

        x :: xs ->
            if liesInside x.position asteroid then
                split asteroid
                    >>= \( asteroids, particles ) ->
                            return ( asteroids, xs, particles, 100 )
            else
                collideAsteroidBullet asteroid xs
                    >>= \( asteroids, xs_, particles, score ) ->
                            return ( asteroids, x :: xs_, particles, score )


collidePlayerAsteroids : Player -> List Asteroid -> State Seed ( Bool, List SegmentParticle )
collidePlayerAsteroids player asteroids =
    case asteroids of
        [] ->
            return ( False, [] )

        x :: xs ->
            collidePlayerAsteroid player x
                >>= \( hitPlayer, particles ) ->
                        if hitPlayer then
                            return ( True, particles )
                        else
                            collidePlayerAsteroids player xs


collidePlayerAsteroid : Player -> Asteroid -> State Seed ( Bool, List SegmentParticle )
collidePlayerAsteroid player asteroid =
    let
        shipTriangles =
            Ship.triangle player.position player.rotation |> Triangle.wrap

        shipSegments =
            concatMap Triangle.segments shipTriangles

        asteroidSegments =
            Asteroids.wrappedSegments asteroid

        segmentPairs =
            pairs shipSegments asteroidSegments

        liesInside_ x =
            Asteroids.liesInside x asteroid
    in
        if
            any (\( x, y ) -> intersect x y) segmentPairs
                || any (\t -> liesInside_ t.a || liesInside_ t.b || liesInside_ t.c) shipTriangles
        then
            segmentParticles player.velocity shipSegments
                >>= \particles ->
                        return ( True, particles )
        else
            return ( False, [] )


pairs : List a -> List b -> List ( a, b )
pairs a b =
    case a of
        [] ->
            []

        x :: xs ->
            pairs_ x b ++ pairs xs b


pairs_ : a -> List b -> List ( a, b )
pairs_ x b =
    case b of
        [] ->
            []

        y :: ys ->
            ( x, y ) :: pairs_ x ys
