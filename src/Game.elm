module Game exposing (Model, Msg, init, update, subscriptions, view)

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
import Tuple2


-- LOCAL IMPORTS

import ComponentUtils exposing (..)
import Bullet
import Asteroid
import AsteroidRandom
import SegmentParticle
import SegmentParticleRandom
import Bounds
import Player
import Vector
import Collisions


-- </editor-fold>
-- <editor-fold> PROGRAM


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- </editor-fold>
-- <editor-fold> MODEL


type GameState
    = Title
    | PreGame
    | Game
    | PostGame
    | GameOver


type alias Model =
    { state : GameState
    , keyboard : Keyboard.Model
    , bullets : List Bullet.Model
    , asteroids : List Asteroid.Model
    , player : Maybe Player.Model
    , segmentParticles : List SegmentParticle.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( keyboard, keyboardCmd ) =
            Keyboard.init

        ( player, playerEffects ) =
            Player.init Vector.zero

        cmds =
            [ Random.generate SpawnAsteroids AsteroidRandom.asteroidGroup
            , Cmd.map KeyboardMsg keyboardCmd
            ]
    in
        { state = Title
        , keyboard = keyboard
        , bullets = []
        , asteroids = []
        , segmentParticles = []
        , player = Just player
        }
            ! cmds
            |> processEffects processPlayerEffect playerEffects



-- </editor-fold>
-- <editor-fold> UPDATE


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Msg
    | PlaySound String
    | SpawnAsteroids (List Asteroid.Model)
    | SpawnBullets (List Bullet.Model)
    | SpawnSegmentParticles (List SegmentParticle.Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg game =
    case msg of
        Tick dt ->
            let
                dtSeconds =
                    Time.inSeconds dt

                ( bullets, bulletEffects ) =
                    game.bullets
                        |> updateGroup (Bullet.update (Bullet.SecondsElapsed dtSeconds))
                        |> Tuple2.mapFst filterAlive

                ( asteroids, asteroidEffects ) =
                    game.asteroids
                        |> updateGroup (Asteroid.update (Asteroid.SecondsElapsed dtSeconds))
                        |> Tuple2.mapFst filterAlive

                ( player, playerEffects ) =
                    updateMaybe
                        (foldlUpdates
                            [ updateIf (Keyboard.isPressed Keyboard.ArrowUp game.keyboard)
                                (Player.update Player.Accelerate)
                            , updateIf (Keyboard.isPressed Keyboard.ArrowDown game.keyboard)
                                (Player.update Player.Decelerate)
                            , updateIf (Keyboard.isPressed Keyboard.ArrowLeft game.keyboard)
                                (Player.update Player.RotateLeft)
                            , updateIf (Keyboard.isPressed Keyboard.ArrowRight game.keyboard)
                                (Player.update Player.RotateRight)
                            , updateIf (Keyboard.isPressed Keyboard.Space game.keyboard)
                                (Player.update Player.FireBullet)
                            , Player.update (Player.SecondsElapsed dtSeconds)
                            ]
                        )
                        game.player

                ( updatedGame, gameCmd ) =
                    { game
                        | bullets = bullets
                        , asteroids = asteroids
                        , player = player
                    }
                        ! []
                        |> processEffects processPlayerEffect playerEffects
                        |> processEffects processAsteroidEffect asteroidEffects
                        |> handleCollisions
            in
                updatedGame ! [ gameCmd ]

        KeyboardMsg keyMsg ->
            let
                ( keyboard, keyboardCmd ) =
                    Keyboard.update keyMsg game.keyboard

                cmds =
                    [ Cmd.map KeyboardMsg keyboardCmd
                    ]
            in
                { game
                    | keyboard = keyboard
                }
                    ! cmds

        PlaySound filename ->
            -- TODO
            game ! []

        SpawnAsteroids asteroids ->
            { game
                | asteroids = game.asteroids ++ asteroids
            }
                ! []

        SpawnBullets bullets ->
            { game
                | bullets = game.bullets ++ bullets
            }
                ! []

        SpawnSegmentParticles segmentParticles ->
            { game
                | segmentParticles = game.segmentParticles ++ segmentParticles
            }
                ! []


filterAlive : List (Maybe a) -> List a
filterAlive =
    List.filterMap identity


ignoreUnusedEffects : List () -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
ignoreUnusedEffects _ =
    identity


processEffects : (effect -> Model -> ( Model, Cmd Msg )) -> List effect -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processEffects processEffectFn effects model =
    let
        processSingleEffect effect ( game, cmd ) =
            let
                ( updatedGame, effectCmd ) =
                    processEffectFn effect game

                batchedCmd =
                    Cmd.batch [ cmd, effectCmd ]
            in
                ( updatedGame, batchedCmd )
    in
        List.foldl processSingleEffect model effects


processPlayerEffect : Player.Effect -> Model -> ( Model, Cmd Msg )
processPlayerEffect effect model =
    case effect of
        Player.PlaySound filename ->
            update (PlaySound filename) model

        Player.SpawnBullet bullet ->
            update (SpawnBullets [ bullet ]) model


processAsteroidEffect : Asteroid.Effect -> Model -> ( Model, Cmd Msg )
processAsteroidEffect effect model =
    case effect of
        Asteroid.SpawnSegmentParticles { velocity, segments } ->
            let
                particlesGenerator =
                    SegmentParticleRandom.particles velocity segments

                cmds =
                    [ Random.generate SpawnSegmentParticles particlesGenerator
                    ]
            in
                model ! cmds

        Asteroid.SpawnSplitAsteroids { fromScale, position } ->
            let
                splitAsteroidsGenerator =
                    AsteroidRandom.asteroidGroupWithScaleAt (fromScale - 1) position

                cmds =
                    [ Random.generate SpawnAsteroids splitAsteroidsGenerator
                    ]
            in
                model ! cmds


handleCollisions : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleCollisions ( model, cmd ) =
    model



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
        scene =
            Collage.group
                [ drawGroup Asteroid.draw game.asteroids
                , drawMaybe Player.draw game.player
                , drawGroup Bullet.draw game.bullets
                ]
    in
        Collage.collage (floor Bounds.width)
            (floor Bounds.height)
            [ Collage.rect Bounds.width Bounds.height |> Collage.filled Color.black
            , scene
            ]
            |> Element.toHtml



-- </editor-fold>
