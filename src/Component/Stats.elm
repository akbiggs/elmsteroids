module Component.Stats exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form, moveY)
import Effects exposing (Effects)
import Game.Update as Update exposing (Update)


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
    | Hide


type alias Effect =
    Effects.None


update : Msg -> Update Model Effect
update msg model =
    case msg of
        NextSector ->
            Update.returnAlive { model | sector = model.sector + 1 }

        IncreaseScore amount ->
            Update.returnAlive { model | score = model.score + amount }

        DecrementNumLives ->
            Update.returnAlive { model | numLives = model.numLives - 1 }

        Hide ->
            Update.returnDead



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
