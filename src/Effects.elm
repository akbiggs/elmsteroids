module Effects exposing (init, map, process, ignoreUnused, andThen, batch)


type alias None =
    ()


init : value -> List effect -> ( value, List effect )
init x effects =
    ( x, effects )


chain : value -> ( value, List effect )
chain x =
    ( x, [] )


add : ( value, List effect ) -> List effect -> ( value, List effect )
add ( x, effects ) newEffects =
    ( x, effects ++ newEffects )


map : (a -> b) -> ( value, List a ) -> ( value, List b )
map fn ( x, effects ) =
    ( x, List.map fn effects )


process : (effectA -> value -> ( value, List effectB )) -> List effectA -> value -> ( value, List effectB )
process effectHandlerFn effects x =
    List.foldl (\effect result -> result `andThen` effectHandlerFn effect)
        (init x)
        effects


ignoreUnused : None -> value -> ( value, List effect )
ignoreUnused _ x =
    ( x, [] )


andThen : ( a, List effect ) -> (a -> ( b, List effect )) -> ( b, List effect )
andThen ( x, effects ) fn =
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
