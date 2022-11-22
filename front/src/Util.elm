module Util exposing (otherwise, third)


otherwise : Maybe a -> Maybe a -> Maybe a
otherwise x y =
    case x of
        Just z ->
            Just z

        Nothing ->
            y


third : ( a, b, c ) -> c
third =
    \( _, _, x ) -> x
