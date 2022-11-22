module Util exposing (checkEmptyList, checkEmptyString, checkListOne, checkMaybe, checkNothing, otherwise, third)


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


checkNothing : Maybe a -> String -> Result String (Maybe a)
checkNothing ma err =
    case ma of
        Nothing ->
            Err err

        Just x ->
            Ok (Just x)


checkMaybe : Maybe a -> String -> Result String a
checkMaybe ma err =
    case ma of
        Nothing ->
            Err err

        Just x ->
            Ok x


checkEmptyString : String -> String -> Result String String
checkEmptyString str err =
    if String.isEmpty str then
        Err err

    else
        Ok str


checkEmptyList : List a -> String -> Result String (List a)
checkEmptyList list err =
    if List.isEmpty list then
        Err err

    else
        Ok list


checkListOne : List a -> String -> Result String a
checkListOne list err =
    if List.length list == 1 then
        List.head list |> Result.fromMaybe ""

    else
        Err err
