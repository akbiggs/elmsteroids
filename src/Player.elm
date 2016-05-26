module Player exposing (Model, tick, draw)

-- <editor-fold> IMPORTS

-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Keyboard.Extra as Keyboard
import Time exposing (Time)

-- LOCAL IMPORTS

import Vector
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

init : Vector -> (Model, Cmd Msg)
init pos =
  { position = pos
  , velocity = Vector.zero
  , velocityDelta = 0
  , rotation = 0
  , rotationDelta = 0
  }

-- </editor-fold>

-- <editor-fold> UPDATE

type Msg
  = SecondsElapsed Float
  | Accelerate
  | Decelerate
  | RotateLeft
  | RotateRight

accel : Float
accel = 57.0

rotationSpeed : Float
rotationSpeed = 1.5

update : Msg -> Model -> (Maybe Model, Cmd Msg)
update msg player =
  case msg of
    SecondsElapsed dt ->
      let
        position =
          add player.position (mulS dt player.velocity)
          |> wrap

        velocity =
          (0, player.velocityDelta * dt)
          |> rotate player.rotation
          |> add player.velocity

        rotation =
          player.rotation + player.rotationDelta * dt
      in
        { player
        | position = position,
        , velocity = velocity,
        , velocityDelta = 0,
        , rotation = rotation
        , rotationDelta = 0
        }

    Accelerate ->
      { player
      | velocityDelta = player.velocityDelta + accel
      }

    Decelerate ->
      { player
      | velocityDelta = player.velocityDelta - accel
      }

    RotateLeft ->
      { player
      | rotationDelta = player.rotationDelta - rotationSpeed
      }

    RotateRight ->
      { player
      | rotationDelta = player.rotationDelta + rotationSpeed
      }

-- </editor-fold>

-- <editor-fold> VIEW

draw : Model -> Form
draw player = Ship.draw player.position player.rotation

-- </editor-fold>
