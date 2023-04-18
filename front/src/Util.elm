module Util exposing (andMapR, checkAllOk, checkEmptyDict, checkEmptyList, checkEmptyString, checkListOne, checkMaybe, checkNothing, chooseIfSingleton, dup, flip, indexOf, otherwise, otherwiseR, third)

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


checkEmptyDict : Dict comparable a -> String -> Result String (Dict comparable a)
checkEmptyDict dict err =
    if Dict.isEmpty dict then
        Err err

    else
        Ok dict


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


dup : (a -> a -> b) -> a -> b
dup f x =
    f x x


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


checkAllOk : (a -> Result String ()) -> List a -> Result String ()
checkAllOk check xs =
    List.foldl
        (\x r ->
            case check x of
                Ok _ ->
                    r

                Err str ->
                    Err str
        )
        (Ok ())
        xs


indexOf : a -> List a -> Maybe Int
indexOf x =
    List.indexedMap Tuple.pair
        >> List.filter (\z -> x == Tuple.second z)
        >> List.head
        >> Maybe.map Tuple.first
