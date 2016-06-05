module Bullet exposing (Model, Msg(..), Effect, init, update, draw)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List exposing (..)
import Collage exposing (Form, group, rect, filled, move, alpha)
import Color exposing (..)
import Vector exposing (..)
import AnimationFrame
import Time exposing (Time)


-- LOCAL IMPORTS
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


type alias Effect =
    ()


update : Msg -> Model -> ( Maybe Model, List Effect )
update msg bullet =
    case msg of
        SecondsElapsed dt ->
            let
                updatedBullet =
                    (moveBullet dt >> killBullet dt) bullet
            in
                ( updatedBullet, [] )


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
