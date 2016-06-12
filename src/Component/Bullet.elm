module Component.Bullet exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List exposing (..)
import Collage exposing (Form, group, rect, filled, move, alpha)
import Color exposing (..)
import Vector exposing (..)
import AnimationFrame
import Time exposing (Time)


-- LOCAL IMPORTS

import Effects exposing (Effects)
import Update


-- </editor-fold>
-- <editor-fold> MODEL


type alias Model =
    { position : Vector
    , velocity : Vector
    , timeUntilDeath : Float
    }


init : Vector -> Vector -> Float -> Model
init pos vel timeUntilDeath =
    { position = pos
    , velocity = vel
    , timeUntilDeath = timeUntilDeath
    }



-- </editor-fold>
-- <editor-fold> UPDATE


type Msg
    = SecondsElapsed Time
    | Explode


type alias Effect =
    Effects.None


update : Msg -> Model -> Update.Result Model Effect
update msg bullet =
    case msg of
        SecondsElapsed dt ->
            bullet
                |> moveBullet dt
                |> killBullet dt
                |> Update.returnMaybe

        Explode ->
            Update.returnNothing


moveBullet : Time -> Model -> Model
moveBullet timeDelta bullet =
    { bullet
        | position = add bullet.position (mulS timeDelta bullet.velocity) |> wrap
    }


killBullet : Time -> Model -> Maybe Model
killBullet timeDelta bullet =
    let
        timeUntilDeath =
            bullet.timeUntilDeath - timeDelta
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
        |> alpha (min bullet.timeUntilDeath 1)



-- </editor-fold>
