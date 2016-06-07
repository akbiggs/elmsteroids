module Component.Update exposing (startOn, startOnMaybe, runOnMaybe, runIf, runOnGroup, filterAliveObjects, andThen)

-- EXTERNAL IMPORTS

import List exposing (map, filter, filterMap, unzip)
import Tuple2


-- LOCAL IMPORTS

import Effects


-- FUNCTIONS


runOnMaybe : (a -> ( Maybe a, List effect )) -> Maybe a -> ( Maybe a, List effect )
runOnMaybe updateFn maybeObj =
    Maybe.map updateFn maybeObj
        |> Maybe.withDefault ( Nothing, [] )


runIf : Bool -> (a -> ( Maybe a, List effect )) -> a -> ( Maybe a, List effect )
runIf cond updateFn obj =
    if cond then
        updateFn obj
    else
        ( Just obj, [] )


runOnGroup : (a -> ( Maybe a, List effect )) -> List a -> ( List (Maybe a), List effect )
runOnGroup updateFn xs =
    List.map updateFn xs
        |> Effects.batch


andThen : ( Maybe a, List effect ) -> (a -> ( Maybe a, List effect )) -> ( Maybe a, List effect )
andThen ( maybeObj, effects ) updateFn =
    let
        ( updatedObj, newEffects ) =
            runOnMaybe updateFn maybeObj
    in
        ( updatedObj, effects ++ newEffects )


filterAliveObjects : ( List (Maybe a), List effect ) -> ( List a, List effect )
filterAliveObjects =
    Tuple2.mapFst (List.filterMap identity)
