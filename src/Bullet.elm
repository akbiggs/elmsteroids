module Bullet exposing (Model, Msg(..), fire, update, draw)

-- <editor-fold> IMPORTS

-- <editor-fold> EXTERNAL IMPORTS

import List exposing (..)
import Collage exposing (Form, group, rect, filled, move, alpha)
import Color exposing (..)
import Vector exposing (..)

-- </editor-fold>

-- <editor-fold> LOCAL IMPORTS

import Ship
import Player
import AnimationFrame
import Time exposing (Time)
import ObjectMsg exposing (..)

-- </editor-fold>

-- </editor-fold>

-- MODEL

type alias Model =
  { position : Vector
  , velocity : Vector
  , timeUntilDeath : Float
  }

fire : Player.Model -> List Model -> List Model
fire player bullets =
  { position = Ship.front player.position player.rotation
  , velocity = player.velocity |> add (rotate player.rotation (0, 80))
  , timeUntilDeath = 3.0
  } :: bullets

-- UPDATE

type Msg
  = SecondsElapsed Time

update : Msg -> Model -> (Maybe Model, Cmd ObjectMsg)
update msg bullet =
  case msg of
    SecondsElapsed dt ->
      (moveBullet dt >> killBullet dt) bullet ! []

moveBullet : Time -> Model -> Model
moveBullet timeDelta bullet =
  { bullet
  | position = add bullet.position (mulS timeDelta bullet.velocity) |> wrap
  }

killBullet : Time -> Model -> Maybe Model
killBullet timeDelta bullet =
  let
    timeUntilDeath =
      bullet.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then
      Just { bullet | timeUntilDeath = timeUntilDeath }
    else
      Nothing

-- VIEW

draw : Model -> Form
draw bullet =
  rect 2 2
    |> filled white
    |> move bullet.position
    |> alpha (min bullet.timeUntilDeath 1)
