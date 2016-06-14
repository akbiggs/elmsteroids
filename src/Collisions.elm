module Collisions exposing (handleCollisions, Effect(..))

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List exposing (map, concat, concatMap, any)
import Game.Update as Update exposing (Update)
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Segment exposing (..)
import Component.Player as Player
import Component.Asteroid as Asteroid exposing (liesInside)
import Component.Bullet as Bullet


-- </editor-fold> END IMPORTS


type alias Objects =
    { player : Maybe Player.Model
    , asteroids : List Asteroid.Model
    , bullets : List Bullet.Model
    }


type Effect
    = PlayerEffect Player.Effect
    | AsteroidEffect Asteroid.Effect
    | BulletEffect Bullet.Effect


handleCollisions : Objects -> Effects Objects Effect
handleCollisions objects =
    let
        ( updatedPlayer, playerEffects ) =
            Update.runOnMaybe (handlePlayerCollisions objects) objects.player

        ( updatedBullets, bulletEffects ) =
            List.map (handleBulletCollisions objects) objects.bullets
                |> Update.filterAlive

        ( updatedAsteroids, asteroidEffects ) =
            List.map (handleAsteroidCollisions objects) objects.asteroids
                |> Update.filterAlive

        effects =
            List.concat
                [ List.map PlayerEffect playerEffects
                , List.map AsteroidEffect asteroidEffects
                , List.map BulletEffect bulletEffects
                ]
    in
        Effects.init
            { player = updatedPlayer
            , asteroids = updatedAsteroids
            , bullets = updatedBullets
            }
            effects


handlePlayerCollisions : Objects -> Update Player.Model Player.Effect
handlePlayerCollisions { asteroids } player =
    if List.any (isPlayerCollidingWithAsteroid player) asteroids then
        Player.update Player.Die player
    else
        Update.returnAlive player


handleBulletCollisions : Objects -> Update Bullet.Model Bullet.Effect
handleBulletCollisions { asteroids } bullet =
    if List.any (isBulletCollidingWithAsteroid bullet) asteroids then
        Bullet.update Bullet.Explode bullet
    else
        Update.returnAlive bullet


handleAsteroidCollisions : Objects -> Update Asteroid.Model Asteroid.Effect
handleAsteroidCollisions { bullets } asteroid =
    if List.any (\x -> isBulletCollidingWithAsteroid x asteroid) bullets then
        Asteroid.update Asteroid.BlowUp asteroid
    else
        Update.returnAlive asteroid


isBulletCollidingWithAsteroid : Bullet.Model -> Asteroid.Model -> Bool
isBulletCollidingWithAsteroid bullet asteroid =
    liesInside bullet.position asteroid


isPlayerCollidingWithAsteroid : Player.Model -> Asteroid.Model -> Bool
isPlayerCollidingWithAsteroid player asteroid =
    let
        shipTriangles =
            Player.wrappedTriangles player

        shipSegments =
            Player.wrappedSegments player

        asteroidSegments =
            Asteroid.wrappedSegments asteroid

        segmentPairs =
            allPairs shipSegments asteroidSegments
    in
        any (uncurry intersect) segmentPairs
            || any
                (\t ->
                    Asteroid.liesInside t.a asteroid
                        || Asteroid.liesInside t.b asteroid
                        || Asteroid.liesInside t.c asteroid
                )
                shipTriangles


allPairs : List a -> List b -> List ( a, b )
allPairs xs ys =
    List.map (\x -> pairsWith x ys) xs
        |> List.concat


pairsWith : a -> List b -> List ( a, b )
pairsWith x ys =
    List.map ((,) x) ys
