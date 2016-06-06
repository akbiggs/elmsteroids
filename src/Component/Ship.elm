module Component.Ship exposing (front, triangle, draw, wrappedSegments, wrappedTriangles)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List
import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color


-- LOCAL IMPORTS

import Vector exposing (Vector)
import Triangle exposing (Triangle)
import Segment exposing (Segment)


-- </editor-fold> END IMPORTS
-- <editor-fold> PROPERTIES


front : Vector -> Float -> Vector
front position rotation =
    Vector.add position (Vector.rotate rotation ( 0, 12 ))


left : Vector -> Float -> Vector
left position rotation =
    Vector.add position (Vector.rotate rotation ( -6, -6 ))


right : Vector -> Float -> Vector
right position rotation =
    Vector.add position (Vector.rotate rotation ( 6, -6 ))


triangle : Vector -> Float -> Triangle
triangle position rotation =
    { a = front position rotation
    , b = left position rotation
    , c = right position rotation
    }


wrappedTriangles : Vector -> Float -> List Triangle
wrappedTriangles position rotation =
    triangle position rotation
        |> Triangle.wrap


wrappedSegments : Vector -> Float -> List Segment
wrappedSegments position rotation =
    wrappedTriangles position rotation
        |> List.concatMap Triangle.segments



-- </editor-fold> END PROPERTIES
-- <editor-fold> VIEW


draw : Vector -> Float -> Form
draw position rotation =
    wrappedTriangles position rotation
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



-- </editor-fold> END VIEW
