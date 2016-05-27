module Player exposing (Model, Msg(..), Effect(..), init, update, draw)

-- <editor-fold> IMPORTS

-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Keyboard.Extra as Keyboard
import Time exposing (Time)

-- LOCAL IMPORTS

import Vector exposing (Vector)
import Ship

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

-- </editor-fold>

-- <editor-fold> UPDATE

type Msg
  = SecondsElapsed Float
  | Accelerate
  | Decelerate
  | RotateLeft
  | RotateRight

type Effect
  = PlaySound String

accel : Float
accel = 57.0

rotationSpeed : Float
rotationSpeed = 1.5

update : Msg -> Model -> (Maybe Model, Cmd Effect)
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

-- </editor-fold>

-- <editor-fold> VIEW

draw : Model -> Form
draw player = Ship.draw player.position player.rotation

-- </editor-fold>
