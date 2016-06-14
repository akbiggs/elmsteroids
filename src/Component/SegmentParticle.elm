module Component.SegmentParticle exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List exposing (map, filterMap)
import Collage exposing (Form, group, path, traced, defaultLine, move, alpha)
import Color exposing (..)
import Effects exposing (Effects)
import Game.Update as Update exposing (Update)
import Time exposing (Time)


-- LOCAL IMPORTS

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
    , timeUntilDeath : Time
    }


type alias InitArgs =
    { position : Vector
    , velocity : Vector
    , rotationVelocity : Float
    , segment : Segment
    , timeUntilDeath : Time
    }


init : InitArgs -> Effects Model Effect
init { position, velocity, rotationVelocity, segment, timeUntilDeath } =
    Effects.return
        { position = position
        , velocity = velocity
        , rotation = 0
        , rotationVelocity = rotationVelocity
        , segment = segment
        , timeUntilDeath = timeUntilDeath
        }



-- </editor-fold> END MODEL
-- <editor-fold> UPDATE


type Msg
    = SecondsElapsed Float


type alias Effect =
    Effects.None


update : Msg -> Update Model Effect
update msg model =
    case msg of
        SecondsElapsed dt ->
            model
                |> moveParticle dt
                |> rotateParticle dt
                |> killParticle dt
                |> Update.returnMaybe


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
killParticle dt particle =
    let
        timeUntilDeath =
            particle.timeUntilDeath - (Time.inSeconds dt)
    in
        if timeUntilDeath > 0 then
            Just { particle | timeUntilDeath = timeUntilDeath }
        else
            Nothing



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw particle =
    particle.segment
        |> Segment.wrap
        |> map (drawSegment particle.rotation)
        |> group
        |> move particle.position
        |> alpha (min (Time.inSeconds particle.timeUntilDeath) 1)


drawSegment : Float -> Segment -> Form
drawSegment rotation segment =
    path
        [ rotate rotation segment.a
        , rotate rotation segment.b
        ]
        |> traced { defaultLine | color = white }



-- </editor-fold> END VIEW
