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

import Update
import Bullet
import Asteroid
import AsteroidRandom
import SegmentParticle
import SegmentParticleRandom
import Bounds
import Player
import Vector
import Collisions
import Star
import StarRandom


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
    , score : Int
    , bullets : List Bullet.Model
    , asteroids : List Asteroid.Model
    , segmentParticles : List SegmentParticle.Model
    , stars : List Star.Model
    , player : Maybe Player.Model
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
            , Random.generate SpawnStars StarRandom.starGroup
            , Cmd.map KeyboardMsg keyboardCmd
            ]
    in
        { state = Title
        , keyboard = keyboard
        , score = 0
        , bullets = []
        , asteroids = []
        , segmentParticles = []
        , stars = []
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
    | IncreaseScore Int
    | SpawnAsteroids (List Asteroid.Model)
    | SpawnBullets (List Bullet.Model)
    | SpawnSegmentParticles (List SegmentParticle.Model)
    | SpawnStars (List Star.Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg game =
    case msg of
        Tick dt ->
            let
                dtSeconds =
                    Time.inSeconds dt

                ( bullets, bulletEffects ) =
                    game.bullets
                        |> Update.group (Bullet.update (Bullet.SecondsElapsed dtSeconds))
                        |> Tuple2.mapFst filterAlive

                ( asteroids, asteroidEffects ) =
                    game.asteroids
                        |> Update.group (Asteroid.update (Asteroid.SecondsElapsed dtSeconds))
                        |> Tuple2.mapFst filterAlive

                ( player, playerEffects ) =
                    Update.maybe
                        (Update.foldl
                            [ Update.onlyIf (Keyboard.isPressed Keyboard.ArrowUp game.keyboard)
                                (Player.update Player.Accelerate)
                            , Update.onlyIf (Keyboard.isPressed Keyboard.ArrowDown game.keyboard)
                                (Player.update Player.Decelerate)
                            , Update.onlyIf (Keyboard.isPressed Keyboard.ArrowLeft game.keyboard)
                                (Player.update Player.RotateLeft)
                            , Update.onlyIf (Keyboard.isPressed Keyboard.ArrowRight game.keyboard)
                                (Player.update Player.RotateRight)
                            , Update.onlyIf (Keyboard.isPressed Keyboard.Space game.keyboard)
                                (Player.update Player.FireBullet)
                            , Player.update (Player.SecondsElapsed dtSeconds)
                            ]
                        )
                        game.player

                ( segmentParticles, segmentParticleEffects ) =
                    game.segmentParticles
                        |> Update.group (SegmentParticle.update (SegmentParticle.SecondsElapsed dtSeconds))
                        |> Tuple2.mapFst filterAlive

                ( stars, starEffects ) =
                    game.stars
                        |> Update.group (Star.update (Star.SecondsElapsed dtSeconds))
                        |> Tuple2.mapFst filterAlive

                ( updatedGame, gameCmd ) =
                    { game
                        | bullets = bullets
                        , asteroids = asteroids
                        , player = player
                        , segmentParticles = segmentParticles
                        , stars = stars
                    }
                        ! []
                        |> processEffects processPlayerEffect playerEffects
                        |> processEffects processAsteroidEffect asteroidEffects
                        |> processEffects processBulletEffect bulletEffects
                        |> processEffects processSegmentParticleEffect segmentParticleEffects
                        |> processEffects processStarEffect starEffects
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

        IncreaseScore amount ->
            { game
                | score = game.score + amount
            }
                ! []

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

        SpawnStars stars ->
            { game
                | stars = game.stars ++ stars
            }
                ! []


filterAlive : List (Maybe a) -> List a
filterAlive =
    List.filterMap identity


ignoreUnusedEffect : () -> Model -> ( Model, Cmd Msg )
ignoreUnusedEffect _ model =
    model ! []


processEffect : (effect -> Model -> ( Model, Cmd Msg )) -> effect -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processEffect processEffectFn effect ( game, cmd ) =
    let
        ( updatedGame, effectCmd ) =
            processEffectFn effect game

        batchedCmd =
            Cmd.batch [ cmd, effectCmd ]
    in
        ( updatedGame, batchedCmd )


processEffects : (effect -> Model -> ( Model, Cmd Msg )) -> List effect -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processEffects processEffectFn effects model =
    -- this function is a simple fold, but the order in which arguments are passed
    -- means we don't have to create a lambda when passing the model in using
    -- the (|>) and (<|) operators.
    List.foldl (processEffect processEffectFn) model effects


processPlayerEffect : Player.Effect -> Model -> ( Model, Cmd Msg )
processPlayerEffect effect model =
    case effect of
        Player.PlaySound filename ->
            update (PlaySound filename) model

        Player.SpawnBullet bullet ->
            update (SpawnBullets [ bullet ]) model

        Player.SpawnSegmentParticles { velocity, segments } ->
            model
                ! [ Random.generate SpawnSegmentParticles
                        <| SegmentParticleRandom.particles velocity segments
                  ]


processAsteroidEffect : Asteroid.Effect -> Model -> ( Model, Cmd Msg )
processAsteroidEffect effect model =
    case effect of
        Asteroid.SpawnSegmentParticles { velocity, segments } ->
            model
                ! [ Random.generate SpawnSegmentParticles
                        <| SegmentParticleRandom.particles velocity segments
                  ]

        Asteroid.SpawnSplitAsteroids { parentScale, position } ->
            model
                ! [ Random.generate SpawnAsteroids
                        <| AsteroidRandom.asteroidGroupWithScaleAt (parentScale - 1) position
                  ]

        Asteroid.IncreaseScore amount ->
            update (IncreaseScore amount) model


processBulletEffect : Bullet.Effect -> Model -> ( Model, Cmd Msg )
processBulletEffect =
    ignoreUnusedEffect


processSegmentParticleEffect : SegmentParticle.Effect -> Model -> ( Model, Cmd Msg )
processSegmentParticleEffect =
    ignoreUnusedEffect


processStarEffect : Star.Effect -> Model -> ( Model, Cmd Msg )
processStarEffect =
    ignoreUnusedEffect


processCollisionEffect : Collisions.Effect -> Model -> ( Model, Cmd Msg )
processCollisionEffect effect =
    case effect of
        Collisions.PlayerEffect playerEffect ->
            processPlayerEffect playerEffect

        Collisions.AsteroidEffect asteroidEffect ->
            processAsteroidEffect asteroidEffect

        Collisions.BulletEffect bulletEffect ->
            processBulletEffect bulletEffect


handleCollisions : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleCollisions ( model, cmd ) =
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
        updatedGame
            ! [ cmd ]
            |> processEffects processCollisionEffect collisionEffects



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
                , [ Update.drawMaybe Player.draw game.player ]
                , List.map Bullet.draw game.bullets
                , List.map SegmentParticle.draw game.segmentParticles
                ]
    in
        Collage.collage (floor Bounds.width) (floor Bounds.height) scene
            |> Element.toHtml



-- </editor-fold> END VIEW
