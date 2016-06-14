module Main exposing (Model, Msg, init, update, subscriptions, view)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Html exposing (Html)
import Html.App
import Collage exposing (Form)
import Element
import Keyboard.Extra as Keyboard
import Random
import Time exposing (Time)
import AnimationFrame
import Color
import Effects exposing (Effects)
import Game.Update as Update exposing (Update)


-- LOCAL IMPORTS

import Component.Bullet as Bullet
import Component.Asteroid as Asteroid
import Component.AsteroidRandom as AsteroidRandom
import Component.SegmentParticle as SegmentParticle
import Component.SegmentParticleRandom as SegmentParticleRandom
import Component.Player as Player
import Component.Star as Star
import Component.StarRandom as StarRandom
import Component.Stats as Stats
import Bounds
import Vector
import Collisions
import DrawUtilities


-- </editor-fold>
-- <editor-fold> PROGRAM


main : Program Never
main =
    Html.App.program
        { init = init |> Effects.toCmd
        , update = \msg model -> update msg model |> Effects.toCmd
        , subscriptions = subscriptions
        , view = view
        }



-- </editor-fold>
-- <editor-fold> MODEL


type GameState
    = Title
    | Game
    | GameOver


type alias Model =
    { state : GameState
    , keyboard : Keyboard.Model
    , stats : Maybe Stats.Model
    , bullets : List Bullet.Model
    , asteroids : List Asteroid.Model
    , segmentParticles : List SegmentParticle.Model
    , stars : List Star.Model
    , player : Maybe Player.Model
    }


init : Effects Model (Cmd Msg)
init =
    let
        ( keyboard, keyboardCmd ) =
            Keyboard.init

        ( player, playerEffects ) =
            Player.init { position = Vector.zero }

        ( stats, statsEffects ) =
            Stats.init { numLives = 3 }
    in
        Effects.init
            { state = Title
            , keyboard = keyboard
            , stats = Just stats
            , bullets = []
            , asteroids = []
            , segmentParticles = []
            , stars = []
            , player = Just player
            }
            [ Random.generate SpawnAsteroids AsteroidRandom.asteroidGroup
            , Random.generate SpawnStars StarRandom.starGroup
            , Cmd.map KeyboardMsg keyboardCmd
            ]
            `Effects.andThen` Effects.handle handlePlayerEffect playerEffects
            `Effects.andThen` Effects.handle handleStatsEffect statsEffects



-- </editor-fold>
-- <editor-fold> UPDATE


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Msg
    | PlaySound String
    | IncreaseScore Int
    | SpawnAsteroids (Effects (List Asteroid.Model) Asteroid.Effect)
    | SpawnBullets (Effects (List Bullet.Model) Bullet.Effect)
    | SpawnSegmentParticles (Effects (List SegmentParticle.Model) SegmentParticle.Effect)
    | SpawnStars (Effects (List Star.Model) Star.Effect)


update : Msg -> Model -> Effects Model (Cmd Msg)
update msg model =
    case msg of
        Tick dt ->
            let
                dtSeconds =
                    Time.inSeconds dt

                ( updatedBullets, bulletEffects ) =
                    List.map (Bullet.update (Bullet.SecondsElapsed dtSeconds)) model.bullets
                        |> Update.filterAlive

                ( updatedAsteroids, asteroidEffects ) =
                    List.map (Asteroid.update (Asteroid.SecondsElapsed dtSeconds)) model.asteroids
                        |> Update.filterAlive

                ( updatedSegmentParticles, segmentParticleEffects ) =
                    List.map (SegmentParticle.update (SegmentParticle.SecondsElapsed dtSeconds)) model.segmentParticles
                        |> Update.filterAlive

                ( updatedStars, starEffects ) =
                    List.map (Star.update (Star.SecondsElapsed dtSeconds)) model.stars
                        |> Update.filterAlive

                ( updatedPlayer, playerEffects ) =
                    Update.runOnMaybe (Player.update (Player.SecondsElapsed dtSeconds)) model.player
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowUp model.keyboard) (Player.update Player.Accelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowDown model.keyboard) (Player.update Player.Decelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowLeft model.keyboard) (Player.update Player.RotateLeft)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowRight model.keyboard) (Player.update Player.RotateRight)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.Space model.keyboard) (Player.update Player.FireBullet)
            in
                Effects.return
                    { model
                        | bullets = updatedBullets
                        , asteroids = updatedAsteroids
                        , player = updatedPlayer
                        , segmentParticles = updatedSegmentParticles
                        , stars = updatedStars
                    }
                    `Effects.andThen` Effects.handle handlePlayerEffect playerEffects
                    `Effects.andThen` Effects.handle handleAsteroidEffect asteroidEffects
                    `Effects.andThen` Effects.handle handleBulletEffect bulletEffects
                    `Effects.andThen` Effects.handle handleSegmentParticleEffect segmentParticleEffects
                    `Effects.andThen` Effects.handle handleStarEffect starEffects
                    `Effects.andThen` handleCollisions

        KeyboardMsg keyMsg ->
            let
                ( keyboard, keyboardCmd ) =
                    Keyboard.update keyMsg model.keyboard
            in
                Effects.init { model | keyboard = keyboard }
                    [ Cmd.map KeyboardMsg keyboardCmd ]

        PlaySound filename ->
            -- TODO
            Effects.return model

        IncreaseScore amount ->
            let
                ( updatedStats, statsEffects ) =
                    Update.runOnMaybe (Stats.update (Stats.IncreaseScore amount)) model.stats
            in
                Effects.return { model | stats = updatedStats }
                    `Effects.andThen` Effects.handle handleStatsEffect statsEffects

        SpawnAsteroids ( asteroids, effects ) ->
            Effects.return { model | asteroids = model.asteroids ++ asteroids }
                `Effects.andThen` Effects.handle handleAsteroidEffect effects

        SpawnBullets ( bullets, effects ) ->
            Effects.return { model | bullets = model.bullets ++ bullets }
                `Effects.andThen` Effects.handle handleBulletEffect effects

        SpawnSegmentParticles ( segmentParticles, effects ) ->
            Effects.return { model | segmentParticles = model.segmentParticles ++ segmentParticles }
                `Effects.andThen` Effects.handle handleSegmentParticleEffect effects

        SpawnStars ( stars, effects ) ->
            Effects.return { model | stars = model.stars ++ stars }
                `Effects.andThen` Effects.handle handleStarEffect effects


handlePlayerEffect : Effects.Handler Player.Effect Model (Cmd Msg)
handlePlayerEffect effect model =
    case effect of
        Player.PlaySound filename ->
            update (PlaySound filename) model

        Player.SpawnBullet bulletAndEffects ->
            update (SpawnBullets (Effects.batch [ bulletAndEffects ])) model

        Player.SpawnSegmentParticles { velocity, segments } ->
            Effects.init model
                [ Random.generate SpawnSegmentParticles
                    <| SegmentParticleRandom.particles velocity segments
                ]


handleAsteroidEffect : Effects.Handler Asteroid.Effect Model (Cmd Msg)
handleAsteroidEffect effect model =
    case effect of
        Asteroid.SpawnSegmentParticles { velocity, segments } ->
            Effects.init model
                [ Random.generate SpawnSegmentParticles
                    <| SegmentParticleRandom.particles velocity segments
                ]

        Asteroid.SpawnSplitAsteroids { parentScale, position } ->
            Effects.init model
                [ Random.generate SpawnAsteroids
                    <| AsteroidRandom.asteroidGroupWithScaleAt (parentScale - 1) position
                ]

        Asteroid.IncreaseScore amount ->
            update (IncreaseScore amount) model


handleBulletEffect : Effects.Handler Bullet.Effect Model (Cmd Msg)
handleBulletEffect =
    Effects.ignoreUnused


handleSegmentParticleEffect : Effects.Handler SegmentParticle.Effect Model (Cmd Msg)
handleSegmentParticleEffect =
    Effects.ignoreUnused


handleStarEffect : Effects.Handler Star.Effect Model (Cmd Msg)
handleStarEffect =
    Effects.ignoreUnused


handleStatsEffect : Effects.Handler Stats.Effect Model (Cmd Msg)
handleStatsEffect =
    Effects.ignoreUnused


handleCollisionEffect : Effects.Handler Collisions.Effect Model (Cmd Msg)
handleCollisionEffect effect model =
    case effect of
        Collisions.PlayerEffect playerEffect ->
            handlePlayerEffect playerEffect model

        Collisions.AsteroidEffect asteroidEffect ->
            handleAsteroidEffect asteroidEffect model

        Collisions.BulletEffect bulletEffect ->
            handleBulletEffect bulletEffect model


handleCollisions : Model -> Effects Model (Cmd Msg)
handleCollisions model =
    let
        objects =
            { player = model.player
            , bullets = model.bullets
            , asteroids = model.asteroids
            }

        ( updatedObjects, collisionEffects ) =
            Collisions.handleCollisions objects

        updatedGame =
            { model
                | player = updatedObjects.player
                , bullets = updatedObjects.bullets
                , asteroids = updatedObjects.asteroids
            }
    in
        Effects.return updatedGame
            `Effects.andThen` Effects.handle handleCollisionEffect collisionEffects



-- </editor-fold>
-- <editor-fold> SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions game =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]



-- </editor-fold>
-- <editor-fold> VIEW


view : Model -> Html Msg
view game =
    let
        background =
            Collage.rect Bounds.width Bounds.height
                |> Collage.filled Color.black

        scene =
            List.concat
                [ [ background ]
                , List.map Star.draw game.stars
                , List.map Asteroid.draw game.asteroids
                , [ DrawUtilities.drawMaybe Player.draw game.player ]
                , List.map Bullet.draw game.bullets
                , List.map SegmentParticle.draw game.segmentParticles
                , [ DrawUtilities.drawMaybe Stats.draw game.stats ]
                ]
    in
        Collage.collage (floor Bounds.width) (floor Bounds.height) scene
            |> Element.toHtml



-- </editor-fold> END VIEW
