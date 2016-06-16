-- <editor-fold> IMPORTS


module Main exposing (..)

import Html exposing (Html)
import Html.App
import Random exposing (..)
import Time exposing (..)
import AnimationFrame exposing (..)
import Collage exposing (Form, collage, group, rect, filled, text, moveY, scale, alpha, scale)
import Element
import Color exposing (..)
import Keyboard.Extra as Keyboard
import State exposing (..)
import DefaultText exposing (..)
import Bounds exposing (..)
import Star
import Player
import Asteroid
import Bullet
import SegmentParticle
import Ship
import Collisions exposing (..)
import Hud
import ComponentUtils exposing (..)


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


type Model
    = Uninitialized
    | Title TitleState
    | PreGame PreGameState
    | Game GameState
    | PostGame PostGameState
    | GameOver GameOverState


type alias TitleState =
    { stars : List Star.Model
    , asteroids : List Asteroid.Model
    , randomSeed : Seed
    , stateTime : Float
    , keyboard : Keyboard.Model
    }


type alias PreGameState =
    { sector : Int
    , score : Int
    , lives : Int
    , stars : List Star.Model
    , asteroids : List Asteroid.Model
    , bullets : List Bullet.Model
    , segmentParticles : List SegmentParticle.Model
    , keyboard : Keyboard.Model
    , randomSeed : Seed
    , stateTime : Float
    }


preGameLength : Float
preGameLength =
    3


type alias GameState =
    { sector : Int
    , score : Int
    , lives : Int
    , stars : List Star.Model
    , player : Player.Model
    , asteroids : List Asteroid.Model
    , bullets : List Bullet.Model
    , segmentParticles : List SegmentParticle.Model
    , keyboard : Keyboard.Model
    , randomSeed : Seed
    , fireTime : Float
    , stateTime : Float
    }


invinciblePeriod : Float
invinciblePeriod =
    3


type alias PostGameState =
    { sector : Int
    , score : Int
    , lives : Int
    , stars : List Star.Model
    , player : Player.Model
    , bullets : List Bullet.Model
    , segmentParticles : List SegmentParticle.Model
    , keyboard : Keyboard.Model
    , randomSeed : Seed
    , stateTime : Float
    }


postGameLength : Float
postGameLength =
    5


type alias GameOverState =
    { sector : Int
    , score : Int
    , stars : List Star.Model
    , asteroids : List Asteroid.Model
    , bullets : List Bullet.Model
    , segmentParticles : List SegmentParticle.Model
    , keyboard : Keyboard.Model
    , randomSeed : Seed
    , stateTime : Float
    }



-- <editor-fold> Constructors


init : ( Model, Cmd ExternalMsg )
init =
    Uninitialized ! []


initTitle : Seed -> ( TitleState, Cmd ExternalMsg )
initTitle randomSeed =
    let
        ( ( stars, asteroids ), randomSeed ) =
            initStarsAndAsteroids randomSeed

        ( keyboard, keyboardCmd ) =
            Keyboard.init

        msgs =
            [ Cmd.map KeyboardUpdate keyboardCmd ]
    in
        { stars = stars
        , asteroids = asteroids
        , randomSeed = randomSeed
        , stateTime = 0
        , keyboard = keyboard
        }
            ! msgs


initStarsAndAsteroids : State Seed ( List Star.Model, List Asteroid.Model )
initStarsAndAsteroids =
    Star.init
        >>= \stars ->
                Asteroid.init
                    >>= \asteroids ->
                            return ( stars, asteroids )


initPreGame : Int -> Int -> Int -> List Star.Model -> List Asteroid.Model -> List Bullet.Model -> List SegmentParticle.Model -> Keyboard.Model -> Seed -> PreGameState
initPreGame sector score lives stars asteroids bullets segmentParticles keyboard randomSeed =
    { sector = sector
    , score = score
    , stars = stars
    , lives = lives
    , asteroids = asteroids
    , bullets = bullets
    , segmentParticles = segmentParticles
    , keyboard = keyboard
    , randomSeed = randomSeed
    , stateTime = 0
    }


initGame : Int -> Int -> Int -> List Star.Model -> List Asteroid.Model -> List Bullet.Model -> List SegmentParticle.Model -> Keyboard.Model -> Seed -> GameState
initGame sector score lives stars asteroids bullets segmentParticles keyboard randomSeed =
    { sector = sector
    , score = score
    , lives = lives
    , stars = stars
    , player =
        { position = ( 0, 0 )
        , velocity = ( 0, 0 )
        , rotation = 0
        }
    , asteroids = asteroids
    , bullets = bullets
    , segmentParticles = segmentParticles
    , keyboard = keyboard
    , randomSeed = randomSeed
    , fireTime = 0
    , stateTime = 0
    }


initPostGame : Int -> Int -> Int -> List Star.Model -> Player.Model -> List Bullet.Model -> List SegmentParticle.Model -> Keyboard.Model -> Seed -> PostGameState
initPostGame sector score lives stars player bullets segmentParticles keyboard randomSeed =
    { sector = sector
    , score = score
    , stars = stars
    , lives = lives
    , player = player
    , bullets = bullets
    , segmentParticles = segmentParticles
    , keyboard = keyboard
    , randomSeed = randomSeed
    , stateTime = 0
    }


initGameOver : Int -> Int -> List Star.Model -> List Asteroid.Model -> List Bullet.Model -> List SegmentParticle.Model -> Keyboard.Model -> Seed -> GameOverState
initGameOver sector score stars asteroids bullets segmentParticles keyboard randomSeed =
    { sector = sector
    , score = score
    , stars = stars
    , asteroids = asteroids
    , bullets = bullets
    , segmentParticles = segmentParticles
    , keyboard = keyboard
    , randomSeed = randomSeed
    , stateTime = 0
    }



-- </editor-fold>
-- </editor-fold>
-- <editor-fold> UPDATE


update : ExternalMsg -> Model -> ( Model, Cmd ExternalMsg )
update msg model =
    case model of
        Uninitialized ->
            case msg of
                Init time ->
                    let
                        randomSeed =
                            inMilliseconds time |> floor |> initialSeed

                        ( titleScreen, titleCmd ) =
                            initTitle randomSeed

                        msgs =
                            [ titleCmd ]
                    in
                        Title titleScreen ! msgs

                _ ->
                    model ! []

        Title titleState ->
            case msg of
                Tick dt ->
                    Title (tickTitle (inSeconds dt) titleState) ! []

                KeyboardUpdate keyMsg ->
                    let
                        ( keyboard, keyboardMsg ) =
                            Keyboard.update keyMsg titleState.keyboard

                        msgs =
                            [ Cmd.map KeyboardUpdate keyboardMsg ]
                    in
                        if Keyboard.isPressed Keyboard.Enter keyboard then
                            PreGame (initPreGame 1 0 3 titleState.stars titleState.asteroids [] [] keyboard titleState.randomSeed) ! msgs
                        else
                            Title
                                { titleState
                                    | keyboard = keyboard
                                }
                                ! msgs

                _ ->
                    model ! []

        PreGame preGameState ->
            case msg of
                Tick dt ->
                    tickPreGame (inSeconds dt) preGameState ! []

                KeyboardUpdate keyMsg ->
                    let
                        ( keyboard, keyboardMsg ) =
                            Keyboard.update keyMsg preGameState.keyboard

                        msgs =
                            [ Cmd.map KeyboardUpdate keyboardMsg ]
                    in
                        PreGame
                            { preGameState
                                | keyboard = keyboard
                            }
                            ! msgs

                _ ->
                    model ! []

        Game gameState ->
            case msg of
                Tick dt ->
                    tickGame (inSeconds dt) gameState ! []

                KeyboardUpdate keyMsg ->
                    let
                        ( keyboard, keyboardMsg ) =
                            Keyboard.update keyMsg gameState.keyboard

                        msgs =
                            [ Cmd.map KeyboardUpdate keyboardMsg ]
                    in
                        Game
                            { gameState
                                | keyboard = keyboard
                            }
                            ! msgs

                _ ->
                    model ! []

        PostGame postGameState ->
            case msg of
                Tick dt ->
                    tickPostGame (inSeconds dt) postGameState ! []

                KeyboardUpdate keyMsg ->
                    let
                        ( keyboard, keyboardMsg ) =
                            Keyboard.update keyMsg postGameState.keyboard

                        msgs =
                            [ Cmd.map KeyboardUpdate keyboardMsg ]
                    in
                        PostGame
                            { postGameState
                                | keyboard = keyboard
                            }
                            ! msgs

                _ ->
                    model ! []

        GameOver gameOverState ->
            case msg of
                Tick dt ->
                    tickGameOver (inSeconds dt) gameOverState ! []

                KeyboardUpdate keyMsg ->
                    let
                        ( keyboard, keyboardMsg ) =
                            Keyboard.update keyMsg gameOverState.keyboard

                        msgs =
                            [ Cmd.map KeyboardUpdate keyboardMsg ]
                    in
                        GameOver
                            { gameOverState
                                | keyboard = keyboard
                            }
                            ! msgs

                _ ->
                    model ! []


tickTitle : Time -> TitleState -> TitleState
tickTitle dt titleState =
    let
        tickMsg =
            Tick dt

        ( asteroids, asteroidCmd ) =
            updateGroup (Asteroid.update tickMsg) titleState.asteroids
    in
        { titleState
            | stars = List.map (Star.tick dt) titleState.stars
            , asteroids = asteroids
            , stateTime = titleState.stateTime + dt
        }


tickPreGame : Time -> PreGameState -> Model
tickPreGame dt preGameState =
    let
        tickMsg =
            Tick dt

        stars =
            List.map (Star.tick dt) preGameState.stars

        ( asteroids, asteroidCmd ) =
            updateGroup (Asteroid.update tickMsg) preGameState.asteroids

        ( bullets, bulletCmd ) =
            updateGroup (Bullet.update tickMsg) preGameState.bullets

        ( ( asteroids', bullets', segmentParticles, _, _ ), randomSeed ) =
            collide Nothing asteroids bullets preGameState.randomSeed

        segmentParticles' =
            List.filterMap (SegmentParticle.tick dt) preGameState.segmentParticles ++ segmentParticles
    in
        if preGameState.stateTime >= preGameLength then
            Game
                (initGame preGameState.sector
                    preGameState.score
                    preGameState.lives
                    stars
                    asteroids'
                    bullets'
                    segmentParticles'
                    preGameState.keyboard
                    randomSeed
                )
        else
            PreGame
                { preGameState
                    | stars = stars
                    , asteroids = asteroids'
                    , bullets = bullets'
                    , segmentParticles = segmentParticles'
                    , randomSeed = randomSeed
                    , stateTime = preGameState.stateTime + dt
                }


tickGame : Time -> GameState -> Model
tickGame dt gameState =
    let
        tickMsg =
            Tick dt

        stars =
            List.map (Star.tick dt) gameState.stars

        player =
            Player.tick dt gameState.keyboard gameState.player

        ( asteroids, asteroidCmd ) =
            updateGroup (Asteroid.update tickMsg) gameState.asteroids

        ( bullets, bulletCmd ) =
            updateGroup (Bullet.update tickMsg) gameState.bullets

        ( bullets', fireTime ) =
            if Keyboard.isPressed Keyboard.Space gameState.keyboard && gameState.fireTime >= 0 then
                ( Bullet.fire gameState.player bullets, -0.3 )
            else
                ( bullets, gameState.fireTime + dt )

        ( ( asteroids', bullets'', segmentParticles, score, hitPlayer ), randomSeed ) =
            collide
                (if gameState.stateTime < invinciblePeriod then
                    Nothing
                 else
                    Just player
                )
                asteroids
                bullets'
                gameState.randomSeed

        score' =
            gameState.score + score

        segmentParticles' =
            List.filterMap (SegmentParticle.tick dt) gameState.segmentParticles ++ segmentParticles
    in
        if hitPlayer then
            let
                lives =
                    gameState.lives - 1
            in
                if lives > 0 then
                    PreGame
                        (initPreGame gameState.sector
                            score'
                            lives
                            stars
                            asteroids'
                            bullets''
                            segmentParticles'
                            gameState.keyboard
                            randomSeed
                        )
                else
                    GameOver
                        (initGameOver gameState.sector
                            score'
                            stars
                            asteroids'
                            bullets''
                            segmentParticles'
                            gameState.keyboard
                            randomSeed
                        )
        else
            case asteroids' of
                [] ->
                    PostGame
                        (initPostGame gameState.sector
                            score'
                            gameState.lives
                            stars
                            player
                            bullets''
                            segmentParticles'
                            gameState.keyboard
                            randomSeed
                        )

                _ ->
                    Game
                        { gameState
                            | score = score'
                            , stars = stars
                            , player = player
                            , asteroids = asteroids'
                            , bullets = bullets''
                            , segmentParticles = segmentParticles'
                            , randomSeed = randomSeed
                            , fireTime = fireTime
                            , stateTime = gameState.stateTime + dt
                        }


tickPostGame : Time -> PostGameState -> Model
tickPostGame dt postGameState =
    let
        tickMsg =
            Tick dt

        stars =
            List.map (Star.tick dt) postGameState.stars

        player =
            Player.tick dt postGameState.keyboard postGameState.player

        ( bullets, cmd ) =
            updateGroup (Bullet.update tickMsg) postGameState.bullets

        segmentParticles =
            List.filterMap (SegmentParticle.tick dt) postGameState.segmentParticles
    in
        if postGameState.stateTime >= postGameLength then
            let
                ( ( stars', asteroids ), randomSeed ) =
                    initStarsAndAsteroids postGameState.randomSeed
            in
                PreGame
                    (initPreGame (postGameState.sector + 1)
                        postGameState.score
                        postGameState.lives
                        stars'
                        asteroids
                        []
                        []
                        postGameState.keyboard
                        randomSeed
                    )
        else
            PostGame
                { postGameState
                    | stars = stars
                    , player = player
                    , bullets = bullets
                    , segmentParticles = segmentParticles
                    , stateTime = postGameState.stateTime + dt
                }


tickGameOver : Time -> GameOverState -> Model
tickGameOver dt gameOverState =
    let
        tickMsg =
            Tick dt

        stars =
            List.map (Star.tick dt) gameOverState.stars

        ( asteroids, asteroidCmd ) =
            updateGroup (Asteroid.update tickMsg) gameOverState.asteroids

        ( bullets, bulletCmd ) =
            updateGroup (Bullet.update tickMsg) gameOverState.bullets

        ( ( asteroids', bullets', segmentParticles, _, _ ), randomSeed ) =
            collide Nothing
                asteroids
                bullets
                gameOverState.randomSeed

        segmentParticles' =
            List.filterMap (SegmentParticle.tick dt) gameOverState.segmentParticles ++ segmentParticles
    in
        GameOver
            { gameOverState
                | stars = stars
                , asteroids = asteroids'
                , bullets = bullets'
                , segmentParticles = segmentParticles'
                , randomSeed = randomSeed
                , stateTime = gameOverState.stateTime + dt
            }



-- </editor-fold>
-- <editor-fold> SUBSCRIPTIONS


subscriptions : Model -> Sub ExternalMsg
subscriptions model =
    case model of
        Uninitialized ->
            times Init

        _ ->
            Sub.batch
                [ diffs Tick
                , Sub.map KeyboardUpdate Keyboard.subscriptions
                ]



-- </editor-fold>
-- <editor-fold> VIEW


view : Model -> Html ExternalMsg
view model =
    let
        scene =
            case model of
                Uninitialized ->
                    group []

                Title titleState ->
                    group
                        [ drawGroup Star.draw titleState.stars
                        , drawGroup Asteroid.draw titleState.asteroids
                        , group
                            [ defaultText 40 "elmsteroids" |> moveY 50
                            , defaultText 16 "github.com/yupferris // 2016" |> moveY -30
                            , defaultText 14 "press enter/return to begin" |> moveY -50
                            ]
                            |> alpha (min titleState.stateTime 1)
                        ]

                PreGame preGameState ->
                    let
                        animAmt =
                            preGameState.stateTime / preGameLength

                        animAmt' =
                            1 - animAmt
                    in
                        group
                            [ drawGroup Star.draw preGameState.stars
                            , drawGroup Asteroid.draw preGameState.asteroids
                              -- Seems there are rendering bugs when drawing the ship with alpha = 0
                            , Ship.draw ( 0, 0 ) ((animAmt' ^ 3) * 8) |> scale (1 + (animAmt' ^ 2) * 2) |> alpha (animAmt |> max 0.00001)
                            , drawGroup Bullet.draw preGameState.bullets
                            , drawGroup SegmentParticle.draw preGameState.segmentParticles
                            , group
                                [ defaultText 26 ("warping to sector " ++ toString preGameState.sector) |> moveY 50
                                , defaultText 18 ("score: " ++ toString preGameState.score ++ " // " ++ Hud.livesText preGameState.lives) |> moveY -30
                                ]
                                |> alpha (min preGameState.stateTime (preGameLength - preGameState.stateTime |> min 1 |> max 0))
                                |> scale (1 + (animAmt' * 0.2))
                            ]

                Game gameState ->
                    group
                        [ drawGroup Star.draw gameState.stars
                        , drawGroup Asteroid.draw gameState.asteroids
                        , let
                            a =
                                if gameState.stateTime < invinciblePeriod then
                                    cos (gameState.stateTime * 50) * 0.4 + 0.6
                                else
                                    1
                          in
                            Player.draw gameState.player |> alpha a
                        , drawGroup Bullet.draw gameState.bullets
                        , drawGroup SegmentParticle.draw gameState.segmentParticles
                        , Hud.draw gameState.sector gameState.score gameState.lives |> alpha (min gameState.stateTime 1)
                        ]

                PostGame postGameState ->
                    group
                        [ drawGroup Star.draw postGameState.stars
                        , Player.draw postGameState.player
                        , drawGroup Bullet.draw postGameState.bullets
                        , drawGroup SegmentParticle.draw postGameState.segmentParticles
                        , group
                            [ defaultText 26 ("sector " ++ toString postGameState.sector ++ " cleared") |> moveY 50
                            , defaultText 18 ("score: " ++ toString postGameState.score ++ " // " ++ Hud.livesText postGameState.lives) |> moveY -30
                            ]
                            |> alpha (min postGameState.stateTime 1)
                        ]

                GameOver gameOverState ->
                    group
                        [ drawGroup Star.draw gameOverState.stars
                        , drawGroup Asteroid.draw gameOverState.asteroids
                        , drawGroup Bullet.draw gameOverState.bullets
                        , drawGroup SegmentParticle.draw gameOverState.segmentParticles
                        , group
                            [ defaultText 36 "GAME OVER" |> moveY 30
                            , defaultText 18 ("sector " ++ toString gameOverState.sector ++ " // score: " ++ toString gameOverState.score) |> moveY -30
                            ]
                            |> alpha (min gameOverState.stateTime 1)
                        ]
    in
        collage (floor width)
            (floor height)
            [ rect width height |> filled black
            , scene
            ]
            |> Element.toHtml



-- </editor-fold>
