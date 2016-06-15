module Component.Ship exposing (Model, front, triangle, draw, wrappedSegments, wrappedTriangles)

-- EXTERNAL IMPORTS

import List
import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Triangle exposing (Triangle)
import Segment exposing (Segment)


-- PROPERTIES


type alias Model =
    { position : Vector
    , rotation : Float
    }


front : Model -> Vector
front { position, rotation } =
    Vector.add position (Vector.rotate rotation ( 0, 12 ))


left : Model -> Vector
left { position, rotation } =
    Vector.add position (Vector.rotate rotation ( -6, -6 ))


right : Model -> Vector
right { position, rotation } =
    Vector.add position (Vector.rotate rotation ( 6, -6 ))


triangle : Model -> Triangle
triangle model =
    { a = front model
    , b = left model
    , c = right model
    }


wrappedTriangles : Model -> List Triangle
wrappedTriangles model =
    triangle model
        |> Triangle.wrap


wrappedSegments : Model -> List Segment
wrappedSegments model =
    wrappedTriangles model
        |> List.concatMap Triangle.segments



-- VIEW


draw : Model -> Form
draw model =
    wrappedTriangles model
        |> List.map
            (\t ->
                let
                    form =
                        polygon [ t.a, t.b, t.c ]
                in
                    group
                        [ form |> filled Color.black
                        , form |> outlined { defaultLine | color = Color.white }
                        ]
            )
        |> group
