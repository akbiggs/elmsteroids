module MaybeUtilities exposing (hasValue)


hasValue : Maybe a -> Bool
hasValue x =
    case x of
        Just _ ->
            True

        Nothing ->
            False
