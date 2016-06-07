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


-- LOCAL IMPORTS

import Component.Update as Update
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
    , stats : Maybe Stats.Model
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

        ( stats, statsEffects ) =
            Stats.init { numLives = 3 }

        cmds =
            [ Random.generate SpawnAsteroids AsteroidRandom.asteroidGroup
            , Random.generate SpawnStars StarRandom.starGroup
            , Cmd.map KeyboardMsg keyboardCmd
            ]
    in
        { state = Title
        , keyboard = keyboard
        , stats = Just stats
        , bullets = []
        , asteroids = []
        , segmentParticles = []
        , stars = []
        , player = Just player
        }
            ! cmds
            |> processEffects processPlayerEffect playerEffects
            |> processEffects processStatsEffect statsEffects



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
update msg model =
    case msg of
        Tick dt ->
            let
                dtSeconds =
                    Time.inSeconds dt

                ( bullets, bulletEffects ) =
                    Update.runOnGroup (Bullet.update (Bullet.SecondsElapsed dtSeconds)) model.bullets
                        |> Update.filterAliveObjects

                ( asteroids, asteroidEffects ) =
                    Update.runOnGroup (Asteroid.update (Asteroid.SecondsElapsed dtSeconds)) model.asteroids
                        |> Update.filterAliveObjects

                ( player, playerEffects ) =
                    Update.startOnMaybe model.player
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowUp model.keyboard) (Player.update Player.Accelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowDown model.keyboard) (Player.update Player.Decelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowLeft model.keyboard) (Player.update Player.RotateLeft)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowRight model.keyboard) (Player.update Player.RotateRight)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.Space model.keyboard) (Player.update Player.FireBullet)
                        `Update.andThen` Player.update (Player.SecondsElapsed dtSeconds)

                ( segmentParticles, segmentParticleEffects ) =
                    Update.runOnGroup (SegmentParticle.update (SegmentParticle.SecondsElapsed dtSeconds)) model.segmentParticles
                        |> Update.filterAliveObjects

                ( stars, starEffects ) =
                    Update.runOnGroup (Star.update (Star.SecondsElapsed dtSeconds)) model.stars
                        |> Update.filterAliveObjects

                ( updatedModel, cmd ) =
                    { model
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
                updatedModel ! [ cmd ]

        KeyboardMsg keyMsg ->
            let
                ( keyboard, keyboardCmd ) =
                    Keyboard.update keyMsg model.keyboard

                cmds =
                    [ Cmd.map KeyboardMsg keyboardCmd
                    ]
            in
                { model
                    | keyboard = keyboard
                }
                    ! cmds

        PlaySound filename ->
            -- TODO
            model ! []

        IncreaseScore amount ->
            let
                ( updatedStats, statsEffects ) =
                    Update.runOnMaybe (Stats.update (Stats.IncreaseScore amount)) model.stats
            in
                { model | stats = updatedStats }
                    ! []
                    |> processEffects processStatsEffect statsEffects

        SpawnAsteroids asteroids ->
            { model | asteroids = model.asteroids ++ asteroids }
                ! []

        SpawnBullets bullets ->
            { model | bullets = model.bullets ++ bullets }
                ! []

        SpawnSegmentParticles segmentParticles ->
            { model | segmentParticles = model.segmentParticles ++ segmentParticles }
                ! []

        SpawnStars stars ->
            { model | stars = model.stars ++ stars }
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


processStatsEffect : Stats.Effect -> Model -> ( Model, Cmd Msg )
processStatsEffect =
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
                , [ DrawUtilities.drawMaybe Player.draw game.player ]
                , List.map Bullet.draw game.bullets
                , List.map SegmentParticle.draw game.segmentParticles
                , [ DrawUtilities.drawMaybe Stats.draw game.stats ]
                ]
    in
        Collage.collage (floor Bounds.width) (floor Bounds.height) scene
            |> Element.toHtml



-- </editor-fold> END VIEW
