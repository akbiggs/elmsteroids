module Star exposing (Model, init, tick, draw)

import List exposing (map)
import Color exposing (..)
import Collage exposing (Form, group, rect, filled, move, alpha)
import Random exposing (Seed, int, float, step)
import State exposing (..)
import Vector exposing (..)
import Bounds exposing (..)

-- MODEL

type alias Model =
  { position : Vector
  , blinkPhase : Float
  , blinkFrequency : Float
  }

init : State Seed (List Model)
init = step (int 80 100) >>= init'

init' : Int -> State Seed (List Model)
init' count =
  if count == 0 then return []
  else (::) <$> initStar <*> init' (count - 1)

initStar : State Seed (Model)
initStar =
  step (float left right) >>= \x ->
    step (float bottom top) >>= \y ->
      step (float 0 (pi * 2)) >>= \phase ->
        step (float 0 2) >>= \frequency ->
          return
            { position = (x, y)
            , blinkPhase = phase
            , blinkFrequency = frequency
            }

tick : Float -> List Model -> List Model
tick timeDelta = map (tickStar timeDelta)

tickStar : Float -> Model -> Model
tickStar timeDelta star =
  { star | blinkPhase = star.blinkPhase + star.blinkFrequency * timeDelta }

draw : List Model -> Form
draw = map drawStar >> group

drawStar : Model -> Form
drawStar star =
  let blink = sin(star.blinkPhase) * 0.4 + 0.6
  in rect 1 1 |> filled white |> move star.position |> alpha blink
