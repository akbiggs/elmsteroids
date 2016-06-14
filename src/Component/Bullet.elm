module Component.Bullet exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form, group, rect, filled, move, alpha)
import Color exposing (..)
import Time exposing (Time)
import Game.Update as Update exposing (Update)
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Vector exposing (..)


-- </editor-fold>
-- <editor-fold> MODEL


type alias Model =
    { position : Vector
    , velocity : Vector
    , timeUntilDeath : Float
    }


type alias InitArgs =
    { position : Vector
    , velocity : Vector
    , timeUntilDeath : Time
    }


init : InitArgs -> Effects Model Effect
init { position, velocity, timeUntilDeath } =
    Effects.return
        { position = position
        , velocity = velocity
        , timeUntilDeath = timeUntilDeath
        }



-- </editor-fold>
-- <editor-fold> UPDATE


type Msg
    = SecondsElapsed Time
    | Explode


type alias Effect =
    Effects.None


update : Msg -> Update Model Effect
update msg model =
    case msg of
        SecondsElapsed dt ->
            model
                |> moveBullet dt
                |> killBullet dt
                |> Update.returnMaybe

        Explode ->
            Update.returnDead


moveBullet : Time -> Model -> Model
moveBullet dt bullet =
    { bullet
        | position = add bullet.position (mulS dt bullet.velocity) |> wrap
    }


killBullet : Time -> Model -> Maybe Model
killBullet dt bullet =
    let
        timeUntilDeath =
            bullet.timeUntilDeath - (dt * Time.second)
    in
        if timeUntilDeath > 0 then
            Just { bullet | timeUntilDeath = timeUntilDeath }
        else
            Nothing



-- </editor-fold>
-- <editor-fold> VIEW


draw : Model -> Form
draw bullet =
    rect 2 2
        |> filled white
        |> move bullet.position
        |> alpha (min (Time.inSeconds bullet.timeUntilDeath) 1)



-- </editor-fold>
