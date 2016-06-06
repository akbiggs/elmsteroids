module Update exposing (maybe, onlyIf, group, map, foldl, drawMaybe)

-- <editor-fold> IMPORTS

import List exposing (map, filter, filterMap, unzip)
import Collage exposing (Form, group)
import Element
import Tuple2


-- </editor-fold> END IMPORTS
-- <editor-fold> FUNCTIONS


maybe : (a -> ( Maybe a, List effect )) -> Maybe a -> ( Maybe a, List effect )
maybe updateFn maybeObj =
    case maybeObj of
        Just obj ->
            updateFn obj

        Nothing ->
            ( Nothing, [] )


onlyIf : Bool -> (a -> ( Maybe a, List effect )) -> a -> ( Maybe a, List effect )
onlyIf cond updateFn obj =
    if cond then
        updateFn obj
    else
        ( Just obj, [] )


map : (a -> ( Maybe a, List effect )) -> ( Maybe a, List effect ) -> ( Maybe a, List effect )
map updateFn ( maybeObj, effects ) =
    let
        ( updatedObj, newEffects ) =
            maybe updateFn maybeObj
    in
        ( updatedObj, effects ++ newEffects )


foldl : List (a -> ( Maybe a, List effect )) -> a -> ( Maybe a, List effect )
foldl updateFns obj =
    List.foldl map ( Just obj, [] ) updateFns


group : (a -> ( b, List effect )) -> List a -> ( List b, List effect )
group updateFn xs =
    List.map updateFn xs
        |> List.unzip
        |> Tuple2.mapSnd List.concat


drawMaybe : (a -> Form) -> Maybe a -> Form
drawMaybe drawFn maybeObj =
    case maybeObj of
        Just obj ->
            drawFn obj

        Nothing ->
            Collage.toForm Element.empty

-- </editor-fold> END FUNCTIONS
