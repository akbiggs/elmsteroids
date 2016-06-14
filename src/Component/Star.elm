module Component.Star exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Color
import Collage exposing (Form, group, rect, filled, move, alpha)
import Effects exposing (Effects)
import Game.Update as Update exposing (Update)


-- LOCAL IMPORTS

import Vector exposing (Vector)


-- </editor-fold> END IMPORTS
-- <editor-fold> MODEL


type alias Model =
    { position : Vector
    , blinkPhase : Float
    , blinkFrequency : Float
    }


type alias InitArgs =
    { position : Vector
    , blinkPhase : Float
    , blinkFrequency : Float
    }


init : InitArgs -> Effects Model Effect
init { position, blinkPhase, blinkFrequency } =
    Effects.return
        { position = position
        , blinkPhase = blinkPhase
        , blinkFrequency = blinkFrequency
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
            Update.returnAlive { model | blinkPhase = model.blinkPhase + model.blinkFrequency * dt }



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw model =
    let
        blink =
            sin (model.blinkPhase) * 0.4 + 0.6
    in
        rect 1 1
            |> filled Color.white
            |> move model.position
            |> alpha blink



-- </editor-fold>
