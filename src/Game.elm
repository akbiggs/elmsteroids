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

import ComponentUtils exposing (..)
import Bullet
import AsteroidRandom
import Asteroid
import ObjectMsg exposing (ObjectMsg)
import Bounds
import Player
import Vector

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
  }

init : (Model, Cmd Msg)
init =
  let
    (keyboard, keyboardCmd) =
      Keyboard.init

    (player, playerCmd) =
      Player.init Vector.zero

    cmds =
      [ Random.generate SpawnAsteroids AsteroidRandom.asteroidGroup
      , Cmd.map KeyboardMsg keyboardCmd
      , Cmd.map processPlayerEffect playerCmd
      ]
  in
    { state = Title
    , keyboard = keyboard
    , bullets = []
    , asteroids = []
    , player = Just player
    } ! cmds

-- </editor-fold>

-- <editor-fold> UPDATE

type Msg
  = Tick Time
  | KeyboardMsg Keyboard.Msg
  | PlaySound String
  | SpawnAsteroids (List Asteroid.Model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg game =
  case msg of
    Tick dt ->
      let
        dtSeconds =
          Time.inSeconds dt

        (bullets, bulletCmd) =
          updateGroup
            (Bullet.update (Bullet.SecondsElapsed dtSeconds))
            game.bullets

        (asteroids, asteroidCmd) =
          updateGroup
            (Asteroid.update (Asteroid.SecondsElapsed dtSeconds))
            game.asteroids

        (player, playerCmd) =
          updateMaybe
            (foldlUpdates
              [ updateIf
                  (Keyboard.isPressed Keyboard.ArrowUp game.keyboard)
                  (Player.update (Player.Accelerate))
              , updateIf
                  (Keyboard.isPressed Keyboard.ArrowDown game.keyboard)
                  (Player.update (Player.Decelerate))
              , updateIf
                  (Keyboard.isPressed Keyboard.ArrowLeft game.keyboard)
                  (Player.update (Player.RotateLeft))
              , updateIf
                  (Keyboard.isPressed Keyboard.ArrowRight game.keyboard)
                  (Player.update (Player.RotateRight))
              , Player.update (Player.SecondsElapsed dtSeconds)
              ]
            )
            game.player

        cmds =
          [ Cmd.map processObjectMsg bulletCmd
          , Cmd.map processObjectMsg asteroidCmd
          , Cmd.map processPlayerEffect playerCmd
          ]
      in
        { game
        | bullets = bullets
        , asteroids = asteroids
        , player = player
        } ! cmds

    KeyboardMsg keyMsg ->
      let
        (keyboard, keyboardCmd) =
          Keyboard.update keyMsg game.keyboard

        cmds =
          [ Cmd.map KeyboardMsg keyboardCmd
          ]
      in
        { game
        | keyboard = keyboard
        } ! cmds

    PlaySound filename ->
      game ! [] -- TODO

    SpawnAsteroids asteroids ->
      { game
      | asteroids = game.asteroids ++ asteroids
      } ! []

processObjectMsg : ObjectMsg -> Msg
processObjectMsg msg =
  case msg of
    ObjectMsg.PlaySound filename ->
      PlaySound filename

processPlayerEffect : Player.Effect -> Msg
processPlayerEffect msg =
  case msg of
    Player.PlaySound filename ->
      PlaySound filename

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
    Collage.collage
      (floor Bounds.width) (floor Bounds.height)
      [ Collage.rect Bounds.width Bounds.height |> Collage.filled Color.black
      , scene
      ]
      |> Element.toHtml

-- </editor-fold>
