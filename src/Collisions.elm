module Collisions exposing (collide)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List exposing (map, concat, concatMap, any)
import Random exposing (Seed)


-- LOCAL IMPORTS

import State exposing (..)
import Segment exposing (..)
import Triangle
import Player
import Asteroid exposing (liesInside)
import Vector exposing (Vector)
import Bullet
import Ship


-- </editor-fold> END IMPORTS


type alias Objects =
    { player : Maybe Player.Model
    , asteroids : List Asteroid.Model
    , bullets : List Bullet.Model
    }


type Collision
    = BulletWithAsteroids Bullet.Model Asteroid.Model
    | PlayerWithAsteroid Player.Model Asteroid.Model


type Effect
    = IncreaseScore Int
    | SpawnSegmentParticles
        { velocity : Vector
        , segments : List Segment
        }


filterCollisions : List (Maybe Collision) -> List Collision
filterCollisions maybeCollisions =
    List.filterMap identity maybeCollisions


handleCollisions : Objects -> ( Objects, List Effect )
handleCollisions { player, asteroids, bullets } =
    let
        playerCollisions =
            case player of
                Just x ->
                    List.map (getPlayerAsteroidCollision x) asteroids
                        |> filterCollisions

                Nothing ->
                    []

        bulletCollisions =
            pairs bullets asteroids
                |> List.map getBulletAsteroidCollision
                |> filterCollisions
    in
        List.concat [ playerCollisions, bulletCollisions ]


getPlayerAsteroidCollision : Player.Model -> Asteroid.Model -> Maybe Collision
getPlayerAsteroidCollision player asteroid =
    if isPlayerCollidingWithAsteroid player asteroid then
        Just <| PlayerWithAsteroid player asteroid
    else
        Nothing


getBulletAsteroidCollision : Bullet.Model -> Asteroid.Model -> Maybe Collision
getBulletAsteroidCollision bullet asteroid =
    if isBulletCollidingWithAsteroid bullet asteroid then
        Just <| BulletWithAsteroid bullet asteroid
    else
        Nothing


collide : Maybe Player.Model -> List Asteroid.Model -> List Bullet.Model -> State Seed ( List Asteroid.Model, List Bullet.Model, List SegmentParticle.Model, Int, Bool )
collide player asteroids bullets =
    collideAsteroidsBullets asteroids bullets
        >>= \( asteroids', bullets', particles, score ) ->
                case player of
                    Just player' ->
                        collidePlayerAsteroids player' asteroids'
                            >>= \( hitPlayer, particles' ) ->
                                    return ( asteroids', bullets', particles ++ particles', score, hitPlayer )

                    _ ->
                        return ( asteroids', bullets', particles, score, False )


collideAsteroidsBullets : List Asteroid.Model -> List Bullet.Model -> State Seed ( List Asteroid.Model, List Bullet.Model, List SegmentParticle.Model, Int )
collideAsteroidsBullets asteroids bullets =
    collideAsteroidsBullets' asteroids bullets
        >>= \( asteroids, bullets', particles, score ) ->
                return ( concat asteroids, bullets', concat particles, score )


collideAsteroidsBullets' : List Asteroid.Model -> List Bullet.Model -> State Seed ( List (List Asteroid.Model), List Bullet.Model, List (List SegmentParticle.Model), Int )
collideAsteroidsBullets' asteroids bullets =
    case asteroids of
        [] ->
            return ( [], bullets, [], 0 )

        x :: xs ->
            collideAsteroidBullet x bullets
                >>= \( asteroids', bullets', particles, score ) ->
                        collideAsteroidsBullets' xs bullets'
                            >>= \( xs', bullets'', particles', score' ) ->
                                    return ( asteroids' :: xs', bullets'', particles :: particles', score + score' )


isBulletCollidingWithAsteroid : Bullet.Model -> Asteroid.Model -> Bool
isBulletCollidingWithAsteroid bullet asteroid =
    liesInside bullet.position asteroid


collideAsteroidBullet : Asteroid.Model -> List Bullet.Model -> State Seed ( List Asteroid.Model, List Bullet.Model, List SegmentParticle.Model, Int )
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
                    >>= \( asteroids, xs', particles, score ) ->
                            return ( asteroids, x :: xs', particles, score )


collidePlayerAsteroids : Player.Model -> List Asteroid.Model -> State Seed ( Bool, List SegmentParticle.Model )
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


isPlayerCollidingWithAsteroid : Player.Model -> Asteroid.Model -> Bool
isPlayerCollidingWithAsteroid player asteroid =
    let
        shipTriangles =
            Ship.triangle player.position player.rotation |> Triangle.wrap

        shipSegments =
            concatMap Triangle.segments shipTriangles

        asteroidSegments =
            Asteroid.wrappedSegments asteroid

        segmentPairs =
            pairs shipSegments asteroidSegments

        isInsideAsteroid x =
            Asteroid.liesInside x asteroid
    in
        any (uncurry intersect) segmentPairs
            || any (\t -> isInsideAsteroid t.a || isInsideAsteroid t.b || isInsideAsteroid t.c) shipTriangles


collidePlayerAsteroid : Player.Model -> Asteroid.Model -> State Seed ( Bool, List SegmentParticle.Model )
collidePlayerAsteroid player asteroid =
    if isPlayerCollidingWithAsteroid player asteroid then
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
            pairs' x b ++ pairs xs b


pairs' : a -> List b -> List ( a, b )
pairs' x b =
    case b of
        [] ->
            []

        y :: ys ->
            ( x, y ) :: pairs' x ys
