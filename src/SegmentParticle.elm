module SegmentParticle exposing (Model, segmentParticles, tick, draw)

import List exposing (map, filterMap)
import Collage exposing (Form, group, path, traced, defaultLine, move, alpha)
import Color exposing (..)
import Random exposing (Seed, float, step)
import State exposing (..)
import Vector exposing (..)
import Segment exposing (Segment, center)

-- MODEL

type alias Model =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  , rotationVelocity : Float
  , segment : Segment
  , timeUntilDeath : Float
  }

segmentParticles : Vector -> List Segment -> State Seed (List Model)
segmentParticles initialVelocity segments =
  case segments of
    [] -> return []
    x::xs ->
      (::)
        <$> segmentParticle initialVelocity x
        <*> segmentParticles initialVelocity xs

segmentParticle : Vector -> Segment -> State Seed Model
segmentParticle initialVelocity segment =
  let angle = float 0 (pi * 2) |> step
  in
    angle >>= \velDirection ->
      step (float 0 40) >>= \velMagnitude ->
        step (float -1 1) >>= \rotationVelocity ->
          step (float 1 3) >>= \timeUntilDeath ->
            let
              position = center segment
              segment' =
                { a = sub segment.a position
                , b = sub segment.b position
                }
            in
              return
                { position = position
                , velocity =
                  rotate velDirection (0, velMagnitude)
                    |> add initialVelocity
                , rotation = 0
                , rotationVelocity = rotationVelocity
                , segment = segment'
                , timeUntilDeath = timeUntilDeath
                }

tick : Float -> Model -> Maybe Model
tick timeDelta =
  moveParticle timeDelta >> rotateParticle timeDelta >> killParticle timeDelta

moveParticle : Float -> Model -> Model
moveParticle timeDelta particle =
  { particle | position =
      add particle.position (mulS timeDelta particle.velocity) |> wrap
  }

rotateParticle : Float -> Model -> Model
rotateParticle timeDelta particle =
  { particle | rotation =
      particle.rotation + particle.rotationVelocity * timeDelta
  }

killParticle : Float -> Model -> Maybe Model
killParticle timeDelta particle =
  let timeUntilDeath = particle.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then
      Just { particle | timeUntilDeath = timeUntilDeath }
    else Nothing

draw : List Model -> Form
draw = map drawParticle >> group

drawParticle : Model -> Form
drawParticle particle =
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
