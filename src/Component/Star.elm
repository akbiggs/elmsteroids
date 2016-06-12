module Component.Star exposing (Model, Msg(..), Effect, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Color
import Collage exposing (Form, group, rect, filled, move, alpha)


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Update


-- </editor-fold> END IMPORTS
-- <editor-fold> MODEL


type alias Model =
    { position : Vector
    , blinkPhase : Float
    , blinkFrequency : Float
    }



-- </editor-fold> END MODEL
-- <editor-fold> UPDATE


type Msg
    = SecondsElapsed Float


type alias Effect =
    Effects.None


update : Msg -> Model -> Update.Result Model Effect
update msg model =
    case msg of
        SecondsElapsed dt ->
            Update.return
                { model
                    | blinkPhase = model.blinkPhase + model.blinkFrequency * dt
                }



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
