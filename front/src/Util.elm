module Util exposing (andMapR, checkEmptyDict, checkEmptyList, checkEmptyString, checkListOne, checkMaybe, checkNothing, chooseIfSingleton, dup, flip, otherwise, otherwiseR, third)

import Dict exposing (Dict)


otherwise : Maybe a -> Maybe a -> Maybe a
otherwise fallback main =
    case main of
        Just _ ->
            main

        Nothing ->
            fallback


otherwiseR : Result e a -> Result e a -> Result e a
otherwiseR fallback main =
    case main of
        Ok _ ->
            main

        Err _ ->
            fallback


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


andMapR : Result err a -> Result err (a -> b) -> Result err b
andMapR =
    -- flipped applicative Tie fighter <*> for Result
    Result.map2 (|>)
