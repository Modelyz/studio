module Scope.State exposing (containsScope, getUpperList)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Typed as T
import Util exposing (otherwise)


getUpperList : Dict String ( Uuid, Type, Maybe Uuid ) -> Scope -> List Scope -> List Scope
getUpperList types scope oldList =
    let
        newList =
            scope :: oldList
    in
    getUpper types scope
        |> Maybe.map (\upperScope -> getUpperList types upperScope newList)
        |> Maybe.withDefault newList


containsScope : Dict String ( Uuid, Type, Maybe Uuid ) -> Scope -> Scope -> Bool
containsScope types inscope outscope =
    case outscope of
        Empty ->
            False

        IsItem (Type.TType outTType) outUuid ->
            case inscope of
                IsItem (Type.TType inTType) inUuid ->
                    outTType == inTType && outUuid == inUuid

                And s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                Or s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                _ ->
                    False

        IsItem (Type.HType outType) outUuid ->
            case inscope of
                IsItem (Type.HType inType) inUuid ->
                    outType == inType && outUuid == inUuid

                And s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                Or s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                _ ->
                    False

        HasUserType (Type.HType outType) outTypeUuid ->
            case inscope of
                IsItem (Type.HType inType) inUuid ->
                    (outType == inType) && H.isAscendantOf inUuid types outTypeUuid

                HasUserType (Type.HType inType) inTypeUuid ->
                    (outType == inType) && H.isAscendantOf inTypeUuid types outTypeUuid

                And s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                Or s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                _ ->
                    False

        HasUserType (Type.TType outType) outTypeUuid ->
            case inscope of
                IsItem (Type.TType inType) inUuid ->
                    (outType == inType) && T.isAscendantOf inUuid types outTypeUuid

                HasUserType (Type.TType inType) inTypeUuid ->
                    (outType == inType) && H.isAscendantOf inTypeUuid types outTypeUuid

                And s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                Or s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                _ ->
                    False

        HasType outType ->
            case inscope of
                HasType inType ->
                    outType == inType

                HasUserType inType _ ->
                    inType == outType

                IsItem inType _ ->
                    outType == inType

                And s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                Or s1 s2 ->
                    containsScope types s1 outscope && containsScope types s2 outscope

                _ ->
                    False

        And s1 s2 ->
            containsScope types inscope s1 && containsScope types inscope s2

        Or s1 s2 ->
            containsScope types inscope s1 || containsScope types inscope s2

        Not s ->
            not (containsScope types inscope s)

        _ ->
            False


getUpper : Dict String ( Uuid, Type, Maybe Uuid ) -> Scope -> Maybe Scope
getUpper types scope =
    case scope of
        IsItem (Type.HType ht) uuid ->
            Dict.get (Uuid.toString uuid) types
                |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (HasUserType (Type.HType ht)) mpuuid)
                |> Maybe.withDefault (HasType (Type.HType ht))
                |> Just

        IsItem (Type.TType tt) uuid ->
            Dict.get (Uuid.toString uuid) types
                |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (HasUserType (Type.TType tt)) mpuuid)
                |> Maybe.withDefault (HasType (Type.TType tt))
                |> Just

        HasType _ ->
            Nothing

        HasUserType t typeUuid ->
            Dict.get (Uuid.toString typeUuid) types
                |> Maybe.map
                    (\( _, _, mpuuid ) ->
                        case mpuuid of
                            Just puuid ->
                                Just (HasUserType t puuid)

                            Nothing ->
                                Just (HasType t)
                    )
                |> Maybe.withDefault (Just (HasType t))

        And s1 s2 ->
            let
                up1 =
                    getUpper types s1

                up2 =
                    getUpper types s2
            in
            Maybe.map2 And up1 up2 |> otherwise up1 |> otherwise up2

        Or s1 s2 ->
            Maybe.map2 Or (getUpper types s1) (getUpper types s2)

        Not _ ->
            Nothing

        Identified _ ->
            Nothing

        Empty ->
            Nothing

        Anything ->
            Nothing
