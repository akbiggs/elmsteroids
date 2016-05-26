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

import CollectionUtils exposing (..)
import Bullet
import AsteroidRandom
import Asteroid
import ObjectMsg exposing (ObjectMsg)
import Bounds

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
  }

init : (Model, Cmd Msg)
init =
  let
    (keyboard, keyboardCmd) =
      Keyboard.init

    cmds =
      [ Cmd.map KeyboardMsg keyboardCmd
      , Random.generate SpawnAsteroids AsteroidRandom.asteroidGroup
      ]
  in
    { state = Title
    , keyboard = keyboard
    , bullets = []
    , asteroids = []
    } ! cmds

-- </editor-fold>

-- <editor-fold> UPDATE

type Msg
  = Tick Time
  | KeyboardMsg Keyboard.Msg
  | PlaySound String
  | SpawnAsteroids (List Asteroid.Model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick dt ->
      let
        dtSeconds =
          Time.inSeconds dt

        (bullets, bulletCmd) =
          updateGroup
            (Bullet.update (Bullet.SecondsElapsed dtSeconds))
            model.bullets

        (asteroids, asteroidCmd) =
          updateGroup
            (Asteroid.update (Asteroid.SecondsElapsed dtSeconds))
            model.asteroids

        cmds =
          [ Cmd.map processObjectMsg bulletCmd
          , Cmd.map processObjectMsg asteroidCmd
          ]
      in
        { model
        | bullets = bullets
        , asteroids = asteroids
        } ! cmds

    KeyboardMsg keyMsg ->
      let
        (keyboard, keyboardCmd) =
          Keyboard.update keyMsg model.keyboard

        cmds =
          [ Cmd.map KeyboardMsg keyboardCmd
          ]
      in
        { model
        | keyboard = keyboard
        } ! cmds

    PlaySound filename ->
      model ! [] -- TODO

    SpawnAsteroids asteroids ->
      { model
      | asteroids = model.asteroids ++ asteroids
      } ! []

processObjectMsg : ObjectMsg -> Msg
processObjectMsg msg =
  case msg of
    ObjectMsg.PlaySound filename ->
      PlaySound filename

-- </editor-fold>

-- <editor-fold> SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs Tick
    , Sub.map KeyboardMsg Keyboard.subscriptions
    ]

-- </editor-fold>

-- <editor-fold> VIEW

view : Model -> Html Msg
view model =
  let
    scene =
      Collage.group
        [ drawGroup Asteroid.draw model.asteroids
        , drawGroup Bullet.draw model.bullets
        ]
  in
    Collage.collage
      (floor Bounds.width) (floor Bounds.height)
      [ Collage.rect Bounds.width Bounds.height |> Collage.filled Color.black
      , scene
      ]
      |> Element.toHtml

-- </editor-fold>
