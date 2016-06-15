module Component.PlayerSpawnAnimation exposing (Model, Msg(..), Effect(..), init, update, draw)

-- EXTERNAL IMPORTS

import Time exposing (Time)
import Game.Update as Update exposing (Update)
import Effects exposing (Effects)
import Collage exposing (Form)


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Component.Ship as Ship
import DrawUtilities


-- MODEL


type alias Model =
    { timeElapsed : Time
    , position : Vector
    }


type alias InitArgs =
    { position : Vector }


init : InitArgs -> Effects Model Effect
init { position } =
    Effects.return
        { position = position
        , timeElapsed = 0
        }


duration : Time
duration =
    3 * Time.second



-- UPDATE


type Msg
    = SecondsElapsed Float


type Effect
    = PlaySound String


update : Msg -> Update Model Effect
update msg model =
    case msg of
        SecondsElapsed dt ->
            let
                timeElapsed =
                    model.timeElapsed + dt * Time.second
            in
                if timeElapsed > duration then
                    Update.returnDead
                else
                    Update.returnAlive { model | timeElapsed = timeElapsed }



-- VIEW


draw : Model -> Form
draw model =
    let
        t =
            min 1 (model.timeElapsed / duration)
    in
        Ship.draw
            { position = ( 0, 0 )
            , rotation = ((1 - t) ^ 3) * 8
            }
            |> Collage.scale (1 + ((1 - t) ^ 2) * 2)
            |> DrawUtilities.alpha' (max 0.00001 t)
