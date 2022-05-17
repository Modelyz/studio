module Page exposing (..)


checkEmptyString : String -> String -> Result String String
checkEmptyString field err =
    if String.isEmpty field then
        Err err

    else
        Ok field


checkEmptyList : List a -> String -> Result String (List a)
checkEmptyList field err =
    if List.isEmpty field then
        Err err

    else
        Ok field


checkNothing : Maybe a -> String -> Result String a
checkNothing field err =
    case field of
        Nothing ->
            Err err

        Just x ->
            Ok x


indexOf : a -> List a -> Maybe Int
indexOf x =
    -- 1st index is 1
    List.indexedMap Tuple.pair >> List.filter (\z -> x == Tuple.second z) >> List.head >> Maybe.map Tuple.first


getItem : Int -> List a -> Maybe a
getItem i =
    List.indexedMap Tuple.pair >> List.filter (\x -> i == Tuple.first x) >> List.head >> Maybe.map Tuple.second


previous : a -> List a -> Maybe a
previous x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i - 1) xs)


next : a -> List a -> Maybe a
next x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i + 1) xs)


isLast : a -> List a -> Bool
isLast x xs =
    indexOf x xs |> Maybe.map ((==) (List.length xs - 1)) |> Maybe.withDefault False


isFirst : a -> List a -> Bool
isFirst x xs =
    indexOf x xs |> Maybe.map ((==) 0) |> Maybe.withDefault False
