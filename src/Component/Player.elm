module Component.Player exposing (Model, Msg(..), Effect(..), init, update, draw, wrappedSegments, wrappedTriangles)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Time exposing (Time)
import Game.Update as Update exposing (Update)


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Component.Ship as Ship
import Component.Bullet as Bullet
import Segment exposing (Segment)
import Triangle exposing (Triangle)
import Effects exposing (Effects)
import DrawUtilities


-- </editor-fold> END IMPORTS
-- <editor-fold> MODEL


type alias Model =
    { position : Vector
    , velocity : Vector
    , velocityDelta : Float
    , rotation : Float
    , rotationDelta : Float
    , timeSinceLastShot : Maybe Time
    , timeSinceSpawning : Time
    }


type alias InitArgs =
    { position : Vector
    }


init : InitArgs -> Effects Model Effect
init { position } =
    Effects.return
        { position = position
        , velocity = Vector.zero
        , velocityDelta = 0
        , rotation = 0
        , rotationDelta = 0
        , timeSinceLastShot = Nothing
        , timeSinceSpawning = 0
        }


accel : Float
accel =
    57.0


rotationSpeed : Float
rotationSpeed =
    1.5


reloadTime : Time
reloadTime =
    0.3 * Time.second


invulnerableTime : Time
invulnerableTime =
    3.0 * Time.second


canFireBullet : Model -> Bool
canFireBullet model =
    case model.timeSinceLastShot of
        Just time ->
            time > reloadTime

        Nothing ->
            True


isInvulnerable : Model -> Bool
isInvulnerable model =
    model.timeSinceSpawning <= invulnerableTime


toShip : Model -> Ship.Model
toShip { position, rotation } =
    { position = position
    , rotation = rotation
    }


front : Model -> Vector
front model =
    Ship.front <| toShip model


wrappedSegments : Model -> List Segment
wrappedSegments model =
    Ship.wrappedSegments <| toShip model


wrappedTriangles : Model -> List Triangle
wrappedTriangles model =
    Ship.wrappedTriangles <| toShip model



-- </editor-fold> END MODEL
-- <editor-fold> UPDATE


type Msg
    = SecondsElapsed Float
    | Accelerate
    | Decelerate
    | RotateLeft
    | RotateRight
    | FireBullet
    | Die


type Effect
    = PlaySound String
    | SpawnBullet (Effects Bullet.Model Bullet.Effect)
    | SpawnSegmentParticles
        { velocity : Vector
        , segments : List Segment
        }


update : Msg -> Update Model Effect
update msg model =
    case msg of
        SecondsElapsed dt ->
            Update.returnAlive
                { model
                    | position =
                        Vector.add model.position (Vector.mulS dt model.velocity)
                            |> Vector.wrap
                    , velocity =
                        ( 0, model.velocityDelta * dt )
                            |> Vector.rotate model.rotation
                            |> Vector.add model.velocity
                    , velocityDelta = 0
                    , rotation = model.rotation + model.rotationDelta * dt
                    , rotationDelta = 0
                    , timeSinceLastShot = Maybe.map (\t -> t + dt * Time.second) model.timeSinceLastShot
                    , timeSinceSpawning = model.timeSinceSpawning + dt * Time.second
                }

        Accelerate ->
            Update.returnAlive { model | velocityDelta = model.velocityDelta + accel }

        Decelerate ->
            Update.returnAlive { model | velocityDelta = model.velocityDelta - accel }

        RotateLeft ->
            Update.returnAlive { model | rotationDelta = -rotationSpeed }

        RotateRight ->
            Update.returnAlive { model | rotationDelta = rotationSpeed }

        FireBullet ->
            if canFireBullet model then
                Update.returnAlive { model | timeSinceLastShot = Just 0 }
                    |> Effects.add [ SpawnBullet (spawnBullet model) ]
            else
                Update.returnAlive model

        Die ->
            if isInvulnerable model then
                Update.returnAlive model
            else
                Update.returnDead
                    |> Effects.add
                        [ SpawnSegmentParticles
                            { velocity = model.velocity
                            , segments = wrappedSegments model
                            }
                        ]


spawnBullet : Model -> Effects Bullet.Model Bullet.Effect
spawnBullet model =
    Bullet.init
        { position = front model
        , velocity =
            model.velocity
                |> Vector.add (Vector.rotate model.rotation ( 0, 80 ))
        , timeUntilDeath = 3.0 * Time.second
        }



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw model =
    model
        |> toShip
        |> Ship.draw
        |> DrawUtilities.alpha' (currentAlpha model)


currentAlpha : Model -> Float
currentAlpha model =
    if isInvulnerable model then
        cos (model.timeSinceSpawning * 20) * 0.4 + 0.6
    else
        1



-- </editor-fold> END VIEW
