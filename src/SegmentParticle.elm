module SegmentParticle exposing (Model, Msg(..), Effect, update, draw)

-- <editor-fold> IMPORTS

-- EXTERNAL IMPORTS

import List exposing (map, filterMap)
import Collage exposing (Form, group, path, traced, defaultLine, move, alpha)
import Color exposing (..)
import Random exposing (Seed, float, step)

-- LOCAL IMPORTS

import State exposing (..)
import Vector exposing (..)
import Segment exposing (Segment, center)

-- </editor-fold> END IMPORTS

-- <editor-fold> MODEL

type alias Model =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  , rotationVelocity : Float
  , segment : Segment
  , timeUntilDeath : Float
  }

-- </editor-fold> END MODEL

-- <editor-fold> UPDATE

type Msg
  = SecondsElapsed Float

type alias Effect =
  ()

update : Msg -> Model -> (Maybe Model, List Effect)
update msg model =
  case msg of
    SecondsElapsed dt ->
      moveParticle dt model
        |> rotateParticle dt
        |> killParticle dt
        |> \x -> (x, [])

moveParticle : Float -> Model -> Model
moveParticle dt particle =
  { particle
  | position =
      add particle.position (mulS dt particle.velocity)
        |> wrap
  }

rotateParticle : Float -> Model -> Model
rotateParticle dt particle =
  { particle
  | rotation =
      particle.rotation + particle.rotationVelocity * dt
  }

killParticle : Float -> Model -> Maybe Model
killParticle timeDelta particle =
  let
    timeUntilDeath =
      particle.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then
      Just { particle | timeUntilDeath = timeUntilDeath }
    else Nothing

-- </editor-fold> END UPDATE

-- <editor-fold> VIEW

draw : Model -> Form
draw particle =
  particle.segment
    |> Segment.wrap
    |> map (drawSegment particle.rotation)
    |> group
    |> move particle.position
    |> alpha (min particle.timeUntilDeath 1)

drawSegment : Float -> Segment -> Form
drawSegment rotation segment =
  path
    [ rotate rotation segment.a
    , rotate rotation segment.b
    ]
    |> traced { defaultLine | color = white }

-- </editor-fold> END VIEW
