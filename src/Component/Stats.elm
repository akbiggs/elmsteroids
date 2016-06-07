module Component.Stats exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form, moveY)


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


init : InitArgs -> ( Model, List Effect )
init { numLives } =
    let
        initializedModel =
            { sector = 1
            , score = 0
            , numLives = numLives
            }
    in
        ( initializedModel, [] )



-- </editor-fold> END MODEL
-- <editor-fold> UPDATE


type Msg
    = NextSector
    | IncreaseScore Int
    | DecrementNumLives
    | Hide


type alias Effect =
    ()


update : Msg -> Model -> ( Maybe Model, List Effect )
update msg model =
    case msg of
        NextSector ->
            ( Just { model | sector = model.sector + 1 }, [] )

        IncreaseScore amount ->
            ( Just { model | score = model.score + amount }, [] )

        DecrementNumLives ->
            ( Just { model | numLives = model.numLives - 1 }, [] )

        Hide ->
            ( Nothing, [] )



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw model =
    defaultText 12 ("sector " ++ toString model.sector ++ " // score: " ++ toString model.score ++ " // " ++ livesText model.numLives)
        |> moveY (top - 10)


livesText : Int -> String
livesText lives =
    case lives of
        1 ->
            "1 ship remains"

        _ ->
            toString lives ++ " ships remain"



-- </editor-fold> END VIEW
