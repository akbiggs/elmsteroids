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
import Component.Star as Star
import Component.StarRandom as StarRandom
import Component.Stats as Stats
import Component.Player as Player
import Component.PlayerSpawnAnimation as PlayerSpawnAnimation
import Component.TitleScreen as TitleScreen
import State.Player as PlayerState
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
    , bullets : List Bullet.Model
    , asteroids : List Asteroid.Model
    , segmentParticles : List SegmentParticle.Model
    , stars : List Star.Model
    , playerState : PlayerState.Model
    , stats : Maybe Stats.Model
    , titleScreen : Maybe TitleScreen.Model
    }


init : Effects Model (Cmd Msg)
init =
    let
        ( keyboard, keyboardCmd ) =
            Keyboard.init

        ( titleScreen, titleScreenEffects ) =
            TitleScreen.init

        ( playerState, playerStateEffects ) =
            PlayerState.init
    in
        Effects.init
            { state = Title
            , keyboard = keyboard
            , stats = Nothing
            , bullets = []
            , asteroids = []
            , segmentParticles = []
            , stars = []
            , playerState = playerState
            , titleScreen = Just titleScreen
            }
            [ Random.generate SpawnAsteroids AsteroidRandom.asteroidGroup
            , Random.generate SpawnStars StarRandom.starGroup
            , Cmd.map KeyboardMsg keyboardCmd
            ]
            `Effects.andThen` Effects.handle handlePlayerStateEffect playerStateEffects
            `Effects.andThen` Effects.handle handleTitleScreenEffect titleScreenEffects


getPlayer : Model -> Maybe Player.Model
getPlayer model =
    case model.playerState of
        PlayerState.Alive player ->
            Just player

        _ ->
            Nothing



-- </editor-fold>
-- <editor-fold> UPDATE


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Msg
    | PlaySound String
    | StartGame
    | IncreaseScore Int
    | NextLife
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

                ( updatedTitleScreen, titleScreenEffects ) =
                    Update.returnMaybe model.titleScreen
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.Enter model.keyboard)
                                            (TitleScreen.update TitleScreen.Dismiss)
                        `Update.andThen` TitleScreen.update (TitleScreen.SecondsElapsed dtSeconds)

                ( updatedPlayerState, playerStateEffects ) =
                    case model.state of
                        Game ->
                            Effects.return model.playerState
                                `Effects.andThen` PlayerState.update (PlayerState.HandleInput model.keyboard)
                                `Effects.andThen` PlayerState.update (PlayerState.SecondsElapsed dtSeconds)

                        _ ->
                            Effects.return model.playerState
            in
                Effects.return
                    { model
                        | bullets = updatedBullets
                        , asteroids = updatedAsteroids
                        , playerState = updatedPlayerState
                        , segmentParticles = updatedSegmentParticles
                        , stars = updatedStars
                        , titleScreen = updatedTitleScreen
                    }
                    `Effects.andThen` Effects.handle handlePlayerStateEffect playerStateEffects
                    `Effects.andThen` Effects.handle handleAsteroidEffect asteroidEffects
                    `Effects.andThen` Effects.handle handleBulletEffect bulletEffects
                    `Effects.andThen` Effects.handle handleSegmentParticleEffect segmentParticleEffects
                    `Effects.andThen` Effects.handle handleStarEffect starEffects
                    `Effects.andThen` Effects.handle handleTitleScreenEffect titleScreenEffects
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

        StartGame ->
            let
                ( stats, statsEffects ) =
                    Stats.init { numLives = 3 }
            in
                Effects.return
                    { model
                        | stats = Just stats
                        , state = Game
                    }
                    `Effects.andThen` Effects.handle handleStatsEffect statsEffects
                    `Effects.andThen` update NextLife

        IncreaseScore amount ->
            let
                ( updatedStats, statsEffects ) =
                    Update.runOnMaybe (Stats.update (Stats.IncreaseScore amount)) model.stats
            in
                Effects.return { model | stats = updatedStats }
                    `Effects.andThen` Effects.handle handleStatsEffect statsEffects

        NextLife ->
            let
                ( updatedStats, statsEffects ) =
                    Update.runOnMaybe (Stats.update Stats.DecrementNumLives) model.stats

                -- TODO: GameOver
                ( updatedPlayerState, playerStateEffects ) =
                    PlayerState.update (PlayerState.RespawnPlayer Vector.zero) model.playerState
            in
                Effects.return
                    { model
                        | stats = updatedStats
                        , playerState = updatedPlayerState
                    }
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


handleCollisions : Model -> Effects Model (Cmd Msg)
handleCollisions model =
    let
        objects =
            { player = getPlayer model
            , bullets = model.bullets
            , asteroids = model.asteroids
            }

        ( updatedObjects, collisionEffects ) =
            Collisions.handleCollisions objects

        ( updatedPlayerState, playerStateEffects ) =
            PlayerState.update (PlayerState.UpdatePlayerStatus updatedObjects.player) model.playerState

        updatedGame =
            { model
                | bullets = updatedObjects.bullets
                , asteroids = updatedObjects.asteroids
                , playerState = updatedPlayerState
            }
    in
        Effects.return updatedGame
            `Effects.andThen` Effects.handle handleCollisionEffect collisionEffects
            `Effects.andThen` Effects.handle handlePlayerStateEffect playerStateEffects


handlePlayerStateEffect : Effects.Handler PlayerState.Effect Model (Cmd Msg)
handlePlayerStateEffect effect model =
    case effect of
        PlayerState.AliveEffect playerEffect ->
            handlePlayerEffect playerEffect model

        PlayerState.SpawningEffect playerSpawnAnimationEffect ->
            handlePlayerSpawnAnimationEffect playerSpawnAnimationEffect model

        PlayerState.DecrementNumLives ->
            update NextLife model


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


handlePlayerSpawnAnimationEffect : Effects.Handler PlayerSpawnAnimation.Effect Model (Cmd Msg)
handlePlayerSpawnAnimationEffect effect model =
    case effect of
        PlayerSpawnAnimation.PlaySound filename ->
            update (PlaySound filename) model


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


handleTitleScreenEffect : Effects.Handler TitleScreen.Effect Model (Cmd Msg)
handleTitleScreenEffect effect model =
    case effect of
        TitleScreen.PlaySound filename ->
            update (PlaySound filename) model

        TitleScreen.StartGame ->
            update StartGame model


handleCollisionEffect : Effects.Handler Collisions.Effect Model (Cmd Msg)
handleCollisionEffect effect model =
    case effect of
        Collisions.PlayerEffect playerEffect ->
            handlePlayerEffect playerEffect model

        Collisions.AsteroidEffect asteroidEffect ->
            handleAsteroidEffect asteroidEffect model

        Collisions.BulletEffect bulletEffect ->
            handleBulletEffect bulletEffect model



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
view model =
    let
        background =
            Collage.rect Bounds.width Bounds.height
                |> Collage.filled Color.black

        scene =
            List.concat
                [ [ background ]
                , List.map Star.draw model.stars
                , List.map Asteroid.draw model.asteroids
                , [ PlayerState.draw model.playerState ]
                , List.map Bullet.draw model.bullets
                , List.map SegmentParticle.draw model.segmentParticles
                , [ DrawUtilities.drawMaybe Stats.draw model.stats ]
                , [ DrawUtilities.drawMaybe TitleScreen.draw model.titleScreen ]
                ]
    in
        Collage.collage (floor Bounds.width) (floor Bounds.height) scene
            |> Element.toHtml



-- </editor-fold> END VIEW
