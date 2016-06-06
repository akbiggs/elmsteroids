module Component.Update exposing (onMaybe, onlyIf, onGroup, combine)

import List exposing (map, filter, filterMap, unzip)
import Effects


onMaybe : (a -> ( Maybe a, List effect )) -> Maybe a -> ( Maybe a, List effect )
onMaybe updateFn maybeObj =
    Maybe.map updateFn maybeObj
        |> Maybe.withDefault ( Nothing, [] )


onlyIf : Bool -> (a -> ( Maybe a, List effect )) -> a -> ( Maybe a, List effect )
onlyIf cond updateFn obj =
    if cond then
        updateFn obj
    else
        ( Just obj, [] )


onGroup : (a -> ( Maybe a, List effect )) -> List a -> ( List (Maybe a), List effect )
onGroup updateFn xs =
    List.map updateFn xs
        |> Effects.batch


combine : List (a -> ( Maybe a, List effect )) -> a -> ( Maybe a, List effect )
combine updateFns obj =
    let
        maybeUpdateFns =
            List.map onMaybe updateFns
    in
        List.foldl Effects.mapWithEffects (Effects.start (Just obj)) maybeUpdateFns
