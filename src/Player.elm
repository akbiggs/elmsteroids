module Player exposing (Model, tick, draw)

import Collage exposing (Form)
import Vector exposing (..)
import Ship
import Time exposing (Time)
import Keyboard.Extra as Keyboard

-- MODEL

type alias Model =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

-- UPDATE

tick : Time -> Keyboard.Model -> Model -> Model
tick timeDelta keyboard player =
  let
    position =
      add player.position (mulS timeDelta player.velocity)
      |> wrap

    accel = 57.0
    upAccel = if Keyboard.isPressed Keyboard.ArrowUp keyboard then accel else 0
    downAccel = if Keyboard.isPressed Keyboard.ArrowDown keyboard then -accel else 0
    velocityDelta = upAccel + downAccel
    velocity =
      (0, velocityDelta * timeDelta)
      |> rotate player.rotation
      |> add player.velocity

    rotationSpeed = 1.5
    leftDelta = if Keyboard.isPressed Keyboard.ArrowLeft keyboard then -rotationSpeed else 0
    rightDelta = if Keyboard.isPressed Keyboard.ArrowRight keyboard then rotationSpeed else 0
    rotationDelta = leftDelta + rightDelta
    rotation = player.rotation + rotationDelta * timeDelta

  in
    { player
      | position = position
      , velocity = velocity
      , rotation = rotation
    }

draw : Model -> Form
draw player = Ship.draw player.position player.rotation
