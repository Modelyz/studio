module Util exposing (otherwise)


otherwise : Maybe a -> Maybe a -> Maybe a
otherwise x y =
    case x of
        Just z ->
            Just z

        Nothing ->
            y
