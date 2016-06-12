module Component.Asteroid exposing (Model, AsteroidSize, Msg(..), Effect(..), update, draw, liesInside, wrappedSegments)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List exposing (map, concatMap, any)
import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color exposing (..)
import Time exposing (Time)


-- LOCAL IMPORTS

import Vector exposing (..)
import Segment exposing (Segment)
import Triangle exposing (Triangle)
import Wrap
import Update


-- </editor-fold> END IMPORTS
-- <editor-fold> MODEL


type alias AsteroidSize =
    ( Int
      -- scale
    , Float
      -- radius
    )


type alias Model =
    { position : Vector
    , velocity : Vector
    , rotation : Float
    , rotationVelocity : Float
    , scale : Int
    , points : List Vector
    }


absolutePoints : Model -> List Vector
absolutePoints model =
    model.points
        |> map (rotate model.rotation >> add model.position)


liesInside : Vector -> Model -> Bool
liesInside point model =
    triangles model
        |> concatMap Triangle.wrap
        |> any (Triangle.liesInside point)


triangles : Model -> List Triangle
triangles model =
    model
        |> segments
        |> map
            (\segment ->
                { a = segment.a
                , b = segment.b
                , c = model.position
                }
            )


segments : Model -> List Segment
segments model =
    let
        points =
            absolutePoints model
    in
        case points of
            [] ->
                []

            x :: _ ->
                segments' x points


segments' : Vector -> List Vector -> List Segment
segments' firstPoint points =
    case points of
        [] ->
            []

        x :: xs ->
            let
                next =
                    case xs of
                        [] ->
                            firstPoint

                        y :: _ ->
                            y

                segment =
                    { a = x
                    , b = next
                    }
            in
                segment :: segments' firstPoint xs


wrappedSegments : Model -> List Segment
wrappedSegments model =
    segments model
        |> concatMap Segment.wrap



-- </editor-fold> END MODEL
-- <editor-fold> UPDATE


type Msg
    = SecondsElapsed Time
    | BlowUp


type Effect
    = SpawnSplitAsteroids
        { position : Vector
        , parentScale : Int
        }
    | SpawnSegmentParticles
        { velocity : Vector
        , segments : List Segment
        }
    | IncreaseScore Int


update : Msg -> Model -> Update.Result Model Effect
update msg model =
    case msg of
        SecondsElapsed dt ->
            model
                |> moveAsteroid dt
                |> rotateAsteroid dt
                |> Update.return

        BlowUp ->
            Update.returnNothing
                |> Effects.add
                    [ SpawnSegmentParticles
                        { velocity = model.velocity
                        , segments = wrappedSegments model
                        }
                    , IncreaseScore 100
                    ]
                |> Effects.addIf (model.scale > 1)
                    [ SpawnSplitAsteroids
                        { position = model.position
                        , parentScale = model.scale
                        }
                    ]


moveAsteroid : Float -> Model -> Model
moveAsteroid dt asteroid =
    { asteroid
        | position = add asteroid.position (mulS dt asteroid.velocity) |> wrap
    }


rotateAsteroid : Float -> Model -> Model
rotateAsteroid dt asteroid =
    { asteroid
        | rotation = asteroid.rotation + asteroid.rotationVelocity * dt
    }



-- </editor-fold> END UPDATE
-- <editor-fold> VIEW


draw : Model -> Form
draw asteroid =
    asteroid
        |> absolutePoints
        |> wrapPoints
        |> map
            (\points ->
                let
                    shape =
                        points |> polygon
                in
                    group
                        [ shape |> filled black
                        , shape |> outlined { defaultLine | color = white }
                        ]
            )
        |> group


wrapPoints : List Vector -> List (List Vector)
wrapPoints =
    let
        move o =
            map (add o)
    in
        Wrap.wrap (\bound -> any (\( x, _ ) -> x < bound))
            (\bound -> any (\( x, _ ) -> x > bound))
            (\bound -> any (\( _, y ) -> y > bound))
            (\bound -> any (\( _, y ) -> y < bound))
            move



-- </editor-fold> END VIEW
