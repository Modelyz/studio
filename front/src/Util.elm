module Util exposing (first, otherwise, second, third)


otherwise : Maybe a -> Maybe a -> Maybe a
otherwise x y =
    case x of
        Just z ->
            Just z

        Nothing ->
            y


first : ( a, b, c ) -> a
first =
    \( x, _, _ ) -> x


second : ( a, b, c ) -> b
second =
    \( _, x, _ ) -> x


third : ( a, b, c ) -> c
third =
    \( _, _, x ) -> x
