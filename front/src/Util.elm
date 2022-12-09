module Util exposing (applyR, checkEmptyList, checkEmptyString, checkListOne, checkMaybe, checkNothing, chooseIfSingleton, flip, otherwise, third)

import Dict exposing (Dict)


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


flip : (a -> b -> c) -> (b -> a -> c)
flip f x y =
    f y x


chooseIfSingleton : List a -> Maybe a
chooseIfSingleton xs =
    if List.length xs == 1 then
        List.head xs

    else
        Nothing


applyR : Result err a -> Result err (a -> b) -> Result err b
applyR rx rf =
    -- flipped applicative <*> for Result
    case rf of
        Ok f ->
            case rx of
                Ok x ->
                    Ok (f x)

                Err err ->
                    Err err

        Err err ->
            Err err
