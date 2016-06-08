module Component.Update exposing (chain, chainMaybe, runOnMaybe, runIf, runOnGroup, filterAliveObjects, andThen)

-- EXTERNAL IMPORTS

import List exposing (map, filter, filterMap, unzip)
import Tuple2


-- LOCAL IMPORTS

import Effects


-- FUNCTIONS


chain : a -> ( Maybe a, List effect )
chain x =
    ( Just x, [] )


chainMaybe : Maybe a -> ( Maybe a, List effect )
chainMaybe maybeX =
    ( maybeX, [] )


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
andThen result updateFn =
    result `Effects.andThen` runOnMaybe updateFn


filterAliveObjects : ( List (Maybe a), List effect ) -> ( List a, List effect )
filterAliveObjects =
    Tuple2.mapFst (List.filterMap identity)
