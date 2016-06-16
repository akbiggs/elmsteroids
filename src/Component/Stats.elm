module Component.Stats exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form, moveY)
import Effects exposing (Effects)


-- LOCAL IMPORTS

import DefaultText exposing (..)
import Bounds exposing (..)


-- </editor-fold> END IMPORTS
-- <editor-fold> MODEL


type alias Model =
    { sector : Int
    , score : Int
    , numLives : Int
    }


type alias InitArgs =
    { numLives : Int
    }


init : InitArgs -> Effects Model Effect
init { numLives } =
    Effects.return
        { sector = 1
        , score = 0
        , numLives = numLives
        }



-- </editor-fold> END MODEL
-- <editor-fold> UPDATE


type Msg
    = NextSector
    | IncreaseScore Int
    | DecrementNumLives


type alias Effect =
    Effects.None


update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of
        NextSector ->
            Effects.return { model | sector = model.sector + 1 }

        IncreaseScore amount ->
            Effects.return { model | score = model.score + amount }

        DecrementNumLives ->
            Effects.return { model | numLives = model.numLives - 1 }



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw model =
    DefaultText.draw 12
        ("sector "
            ++ toString model.sector
            ++ " // score: "
            ++ toString model.score
            ++ " // "
            ++ livesText model.numLives
        )
        |> moveY (top - 10)


livesText : Int -> String
livesText lives =
    case lives of
        1 ->
            "1 ship remains"

        _ ->
            toString lives ++ " ships remain"



-- </editor-fold> END VIEW
