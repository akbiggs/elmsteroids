module Star exposing (Model, Msg(..), Effect, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Color
import Collage exposing (Form, group, rect, filled, move, alpha)


-- LOCAL IMPORTS

import Vector exposing (Vector)


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
                blinkPhase =
                  model.blinkPhase + model.blinkFrequency * dt

                updatedModel =
                    { model
                        | blinkPhase = blinkPhase
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
