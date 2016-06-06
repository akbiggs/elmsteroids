module Effects exposing (start, map, mapWithEffects, batch)


start : a -> ( a, List effect )
start x =
    ( x, [] )


map : (a -> b) -> ( a, List effect ) -> ( b, List effect )
map fn ( x, effects ) =
    ( fn x, effects )


mapWithEffects : (a -> ( b, List effect )) -> ( a, List effect ) -> ( b, List effect )
mapWithEffects fn ( x, effects ) =
    let
        ( y, newEffects ) =
            fn x
    in
        ( y, effects ++ newEffects )


batch : List ( a, List effect ) -> ( List a, List effect )
batch results =
    let
        ( xs, effectsLists ) =
            List.unzip results
    in
        ( xs, List.concat effectsLists )
