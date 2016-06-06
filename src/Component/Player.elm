module Component.Player exposing (Model, Msg(..), Effect(..), init, update, draw, wrappedSegments, wrappedTriangles)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form)


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Component.Ship as Ship
import Component.Bullet as Bullet
import Segment exposing (Segment)
import Triangle exposing (Triangle)


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


init : Vector -> ( Model, List Effect )
init pos =
    let
        initialModel =
            { position = pos
            , velocity = Vector.zero
            , velocityDelta = 0
            , rotation = 0
            , rotationDelta = 0
            , reloadTime = 0
            }
    in
        ( initialModel, [] )


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
    | SpawnSegmentParticles
        { velocity : Vector
        , segments : List Segment
        }


update : Msg -> Model -> ( Maybe Model, List Effect )
update msg model =
    case msg of
        SecondsElapsed dt ->
            let
                position =
                    Vector.add model.position (Vector.mulS dt model.velocity)
                        |> Vector.wrap

                velocity =
                    ( 0, model.velocityDelta * dt )
                        |> Vector.rotate model.rotation
                        |> Vector.add model.velocity

                rotation =
                    model.rotation + model.rotationDelta * dt

                reloadTime =
                    max (model.reloadTime - dt) 0

                updatedModel =
                    { model
                        | position = position
                        , velocity = velocity
                        , velocityDelta = 0
                        , rotation = rotation
                        , rotationDelta = 0
                        , reloadTime = reloadTime
                    }
            in
                ( Just updatedModel, [] )

        Accelerate ->
            let
                updatedModel =
                    { model
                        | velocityDelta = model.velocityDelta + accel
                    }
            in
                ( Just updatedModel, [] )

        Decelerate ->
            let
                updatedModel =
                    { model
                        | velocityDelta = model.velocityDelta - accel
                    }
            in
                ( Just updatedModel, [] )

        RotateLeft ->
            let
                updatedModel =
                    { model
                        | rotationDelta = -rotationSpeed
                    }
            in
                ( Just updatedModel, [] )

        RotateRight ->
            let
                updatedModel =
                    { model
                        | rotationDelta = rotationSpeed
                    }
            in
                ( Just updatedModel, [] )

        FireBullet ->
            if model.reloadTime > 0 then
                ( Just model, [] )
            else
                let
                    bullet =
                        Bullet.init (front model)
                            (model.velocity |> Vector.add (Vector.rotate model.rotation ( 0, 80 )))
                            3.0

                    updatedModel =
                        { model
                            | reloadTime = 0.3
                        }

                    effects =
                        [ SpawnBullet bullet
                        ]
                in
                    ( Just updatedModel, effects )

        Die ->
            let
                segmentParticleEffect =
                    SpawnSegmentParticles
                        { velocity = model.velocity
                        , segments = wrappedSegments model
                        }

                effects =
                    [ segmentParticleEffect ]
            in
                ( Nothing, effects )



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw player =
    Ship.draw player.position player.rotation



-- </editor-fold> END VIEW
