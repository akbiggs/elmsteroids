module Asteroid exposing (Model, AsteroidSize, Msg(..), update, draw, liesInside, wrappedSegments)

-- <editor-fold> IMPORTS

-- <editor-fold> EXTERNAL IMPORTS

import List exposing (map, concatMap, any)
import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color exposing (..)
import Random exposing (Seed, int, float, step)
import Time exposing (Time)

-- </editor-fold>

-- <editor-fold> LOCAL IMPORTS

import State exposing (..)
import Vector exposing (..)
import Segment exposing (Segment)
import Triangle exposing (Triangle)
import Bounds
import SegmentParticle exposing (segmentParticles)
import ObjectMsg exposing (..)
import Wrap

-- </editor-fold>

-- </editor-fold>

-- <editor-fold> MODEL

type alias AsteroidSize =
  ( Int   -- scale
  , Float -- radius
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
absolutePoints asteroid =
  asteroid.points
    |> map (rotate asteroid.rotation >> add asteroid.position)

liesInside : Vector -> Model -> Bool
liesInside point =
  triangles
    >> concatMap Triangle.wrap
    >> any (Triangle.liesInside point)

triangles : Model -> List Triangle
triangles asteroid =
  asteroid
    |> segments
    |> map
       (\segment ->
          { a = segment.a
          , b = segment.b
          , c = asteroid.position
          })

segments : Model -> List Segment
segments asteroid =
  let
    points =
      absolutePoints asteroid
  in
    case points of
      [] ->
        []

      x::_ ->
        segments' x points

segments' : Vector -> List Vector -> List Segment
segments' firstPoint points =
  case points of
    [] ->
      []

    x::xs ->
      let
        next =
          case xs of
            [] ->
              firstPoint

            y::_ ->
              y

        segment =
          { a = x
          , b = next
          }
      in
        segment :: segments' firstPoint xs

wrappedSegments : Model -> List Segment
wrappedSegments =
  segments >> concatMap Segment.wrap

-- split : Model -> State Seed (List Model, List SegmentParticle.Model)
-- split asteroid =
--   segmentParticles asteroid.velocity (segments asteroid) >>= \particles ->
--     let
--       scale =
--         asteroid.scale - 1
--     in
--       if size > 0 then
--         Random.step (Random.int 1 3) >>=
--           split' asteroid.position scale >>= \asteroids ->
--             return (asteroids, particles)
--       else
--         return ([], particles)

-- </editor-fold>

-- <editor-fold> UPDATE

type Action
  = SecondsElapsed Time

type alias Effect =
  ()

update : Action -> Model -> (Maybe Model, List Effect)
update msg asteroid =
  case msg of
    SecondsElapsed dt ->
      let
        updatedAsteroid =
          Just (moveAsteroid dt asteroid |> rotateAsteroid dt)
      in
        (updatedAsteroid, [])

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

-- </editor-fold>

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
            ])
    |> group

wrapPoints : List Vector -> List (List Vector)
wrapPoints =
  let
    move o =
      map (add o)
  in
    Wrap.wrap
      (\bound -> any (\(x, _) -> x < bound))
      (\bound -> any (\(x, _) -> x > bound))
      (\bound -> any (\(_, y) -> y > bound))
      (\bound -> any (\(_, y) -> y < bound))
      move

-- </editor-fold>
