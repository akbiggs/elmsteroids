module Game.Update exposing (Result, Update, return, returnMaybe, returnNothing, runOnMaybe, runIf, batchEffectsAndFilterAlive, andThen)

-- EXTERNAL IMPORTS

import List exposing (map, filter, filterMap, unzip)


-- LOCAL IMPORTS

import Effects exposing (Effects)


-- TYPES


type alias Result a effect =
    Effects (Maybe a) effect


type alias Update a effect =
    a -> Result a effect



-- FUNCTIONS


return : a -> Result a effect
return x =
    Effects.return (Just x)


returnMaybe : Maybe a -> Result a effect
returnMaybe maybeX =
    Effects.return maybeX


returnNothing : Result a effect
returnNothing =
    Effects.return Nothing


runOnMaybe : Update a effect -> Maybe a -> Result a effect
runOnMaybe updateFn maybeObj =
    Maybe.map updateFn maybeObj
        |> Maybe.withDefault ( Nothing, [] )


runIf : Bool -> Update a effect -> a -> Result a effect
runIf cond updateFn obj =
    if cond then
        updateFn obj
    else
        ( Just obj, [] )


andThen : Result a effect -> Update a effect -> Result a effect
andThen result updateFn =
    result `Effects.andThen` runOnMaybe updateFn


batchEffectsAndFilterAlive : List (Result a effect) -> Effects (List a) effect
batchEffectsAndFilterAlive results =
    let
        ( maybeValues, effects ) =
            Effects.batch results
    in
        Effects.init (List.filterMap identity maybeValues) effects
