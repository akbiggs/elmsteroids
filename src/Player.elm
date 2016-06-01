module Player exposing (Model, Msg(..), Effect(..), init, update, draw)

-- <editor-fold> IMPORTS

-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Keyboard.Extra as Keyboard
import Time exposing (Time)

-- LOCAL IMPORTS

import Vector exposing (Vector)
import Ship
import Bullet

-- </editor-fold>

-- <editor-fold> MODEL

type alias Model =
  { position : Vector
  , velocity : Vector
  , velocityDelta : Float
  , rotation : Float
  , rotationDelta : Float
  }

init : Vector -> (Model, Cmd Effect)
init pos =
  { position = pos
  , velocity = Vector.zero
  , velocityDelta = 0
  , rotation = 0
  , rotationDelta = 0
  } ! []

accel : Float
accel = 57.0

rotationSpeed : Float
rotationSpeed = 1.5

-- </editor-fold>

-- <editor-fold> UPDATE

type Action
  = SecondsElapsed Float
  | Accelerate
  | Decelerate
  | RotateLeft
  | RotateRight
  | FireBullet

type Effect
  = PlaySound String
  | SpawnBullet Bullet.Model

update : Action -> Model -> (Maybe Model, List Effect)
update msg player =
  case msg of
    SecondsElapsed dt ->
      let
        position =
          Vector.add player.position (Vector.mulS dt player.velocity)
          |> Vector.wrap

        velocity =
          (0, player.velocityDelta * dt)
          |> Vector.rotate player.rotation
          |> Vector.add player.velocity

        rotation =
          player.rotation + player.rotationDelta * dt
      in
        Just
          { player
          | position = position
          , velocity = velocity
          , velocityDelta = 0
          , rotation = rotation
          , rotationDelta = 0
          } ! []

    Accelerate ->
      Just
        { player
        | velocityDelta = player.velocityDelta + accel
        } ! []

    Decelerate ->
      Just
        { player
        | velocityDelta = player.velocityDelta - accel
        } ! []

    RotateLeft ->
      Just
        { player
        | rotationDelta = -rotationSpeed
        } ! []

    RotateRight ->
      Just
        { player
        | rotationDelta = rotationSpeed
        } ! []

    FireBullet ->
      let
        bullet =
          Bullet.init
            (Ship.front player.position player.rotation)
            (player.velocity |> add (rotate player.rotation (0, 80)))
            3.0

        effects =
          [ SpawnBullet bullet
          ]
      in
        (Just player, effects)

-- </editor-fold>

-- <editor-fold> VIEW

draw : Model -> Form
draw player = Ship.draw player.position player.rotation

-- </editor-fold>
