module Star exposing (Model, Msg(..), Effect, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List
import Color
import Collage exposing (Form, group, rect, filled, move, alpha)
import Random


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Bounds


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
    ()


update : Msg -> Model -> ( Maybe Model, List Effect )
update msg model =
    case msg of
        SecondsElapsed dt ->
            let
                updatedModel =
                    { model
                        | blinkPhase = model.blinkPhase + model.blinkFrequency * dt
                    }
            in
                ( Just updatedModel, [] )



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
