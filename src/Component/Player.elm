module Component.Player exposing (Model, Msg(..), Effect(..), init, update, draw, wrappedSegments, wrappedTriangles)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Random exposing (Generator)


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Component.Ship as Ship
import Component.Bullet as Bullet
import Component.SegmentParticle as SegmentParticle
import Component.SegmentParticleRandom as SegmentParticleRandom
import Segment exposing (Segment)
import Triangle exposing (Triangle)
import Effects exposing (Effects)
import Update


-- </editor-fold> END IMPORTS
-- <editor-fold> MODEL


type alias Model =
    { position : Vector
    , velocity : Vector
    , velocityDelta : Float
    , rotation : Float
    , rotationDelta : Float
    , reloadTime : Float
    }


init : Vector -> Effects Model Effect
init pos =
    Effects.return
        { position = pos
        , velocity = Vector.zero
        , velocityDelta = 0
        , rotation = 0
        , rotationDelta = 0
        , reloadTime = 0
        }


accel : Float
accel =
    57.0


rotationSpeed : Float
rotationSpeed =
    1.5


front : Model -> Vector
front model =
    Ship.front model.position model.rotation


wrappedSegments : Model -> List Segment
wrappedSegments model =
    Ship.wrappedSegments model.position model.rotation


wrappedTriangles : Model -> List Triangle
wrappedTriangles model =
    Ship.wrappedTriangles model.position model.rotation



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
    | SpawnBullet Bullet.Model
    | GenerateSegmentParticles (Generator (List SegmentParticle.Model))


update : Msg -> Model -> Update.Result Effect Model
update msg model =
    case msg of
        SecondsElapsed dt ->
            Update.return
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
                    , reloadTime = max (model.reloadTime - dt) 0
                }

        Accelerate ->
            Update.return { model | velocityDelta = model.velocityDelta + accel }

        Decelerate ->
            Update.return { model | velocityDelta = model.velocityDelta - accel }

        RotateLeft ->
            Update.return { model | rotationDelta = -rotationSpeed }

        RotateRight ->
            Update.return { model | rotationDelta = rotationSpeed }

        FireBullet ->
            if model.reloadTime <= 0 then
                Update.return model
            else
                Update.return
                    { model
                        | reloadTime = 0.3
                    }
                    |> Effects.add [ SpawnBullet (spawnBullet model) ]

        Die ->
            Update.returnNothing
                |> Effects.add [ GenerateSegmentParticles (deathParticles model) ]


spawnBullet : Model -> Bullet.Model
spawnBullet model =
    Bullet.init (front model)
        (model.velocity |> Vector.add (Vector.rotate model.rotation ( 0, 80 )))
        3.0


deathParticles : Model -> Generator (List SegmentParticle.Model)
deathParticles model =
    SegmentParticleRandom.particles (model.velocity) (wrappedSegments model)



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw player =
    Ship.draw player.position player.rotation



-- </editor-fold> END VIEW
