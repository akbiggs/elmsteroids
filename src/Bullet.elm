module Bullet exposing (Model, fire, update, draw)

-- IMPORTS

import List exposing (..)
import Collage exposing (Form, group, rect, filled, move, alpha)
import Color exposing (..)
import Vector exposing (..)
import Ship
import Player
import AnimationFrame
import Time exposing (Time)
import ExternalMsg exposing (..)

-- MODEL

type alias Model =
  { position : Vector
  , velocity : Vector
  , timeUntilDeath : Float
  }

fire : Player.Model -> List Model -> List Model
fire player bullets =
  { position = Ship.front player.position player.rotation
  , velocity = player.velocity
      |> add (rotate player.rotation (0, 80))
  , timeUntilDeath = 3.0
  } :: bullets

-- UPDATE

update : ExternalMsg -> Model -> (Maybe Model, Cmd ExternalMsg)
update msg bullet =
  case msg of
    Tick dt ->
      (moveBullet dt >> killBullet dt) bullet ! []

    _ ->
      Just bullet ! []

moveBullet : Time -> Model -> Model
moveBullet timeDelta bullet =
  { bullet | position =
      add bullet.position (mulS timeDelta bullet.velocity) |> wrap
  }

killBullet : Time -> Model -> Maybe Model
killBullet timeDelta bullet =
  let timeUntilDeath = bullet.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then
      Just { bullet | timeUntilDeath = timeUntilDeath }
    else Nothing

-- VIEW

draw : Model -> Form
draw bullet =
  rect 2 2
  |> filled white
  |> move bullet.position
  |> alpha (min bullet.timeUntilDeath 1)
