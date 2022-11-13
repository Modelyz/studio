module Scope.State exposing (..)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Scope.Scope exposing (Scope(..))
import Type
import Typed.Type as TType
import Typed.Typed as T exposing (Typed)
import Util exposing (otherwise)


getUpperList : Dict String (Typed a) -> Dict String (Hierarchic b) -> Scope -> List Scope -> List Scope
getUpperList allT allH scope oldList =
    let
        newList =
            scope :: oldList
    in
    getUpper allT allH scope
        |> Maybe.map (\upperScope -> getUpperList allT allH upperScope newList)
        |> Maybe.withDefault newList


containsScope : Dict String (Typed a) -> Dict String (Hierarchic b) -> Scope -> Scope -> Bool
containsScope allT allH inscope outscope =
    case outscope of
        Empty ->
            False

        IsItem (Type.TType outTType) outUuid ->
            case inscope of
                IsItem (Type.TType inTType) inUuid ->
                    outTType == inTType && outUuid == inUuid

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        IsItem (Type.HType outType) outUuid ->
            case inscope of
                IsItem (Type.HType inType) inUuid ->
                    outType == inType && outUuid == inUuid

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        HasUserType (Type.HType outType) _ outParentUuid ->
            case inscope of
                IsItem (Type.HType inType) inUuid ->
                    (outType == inType)
                        && (Maybe.map3 H.isAscendantOf (H.find allH inUuid) (Just allH) (H.find allH outParentUuid)
                                |> Maybe.withDefault False
                           )

                HasUserType (Type.HType inType) inParentType inParentUuid ->
                    (outType == inType)
                        && (Maybe.map3 H.isAscendantOf (H.find allH inParentUuid) (Just allH) (H.find allH outParentUuid)
                                |> Maybe.withDefault False
                           )

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        HasUserType (Type.TType outType) _ outParentUuid ->
            case inscope of
                IsItem (Type.TType inType) inUuid ->
                    (outType == inType)
                        && (Maybe.map3 T.isAscendantOf (T.find allT inUuid) (Just allH) (H.find allH outParentUuid)
                                |> Maybe.withDefault False
                           )

                HasUserType (Type.TType inType) inParentType inParentUuid ->
                    (outType == inType)
                        && (Maybe.map3 H.isAscendantOf (H.find allH inParentUuid) (Just allH) (H.find allH outParentUuid)
                                |> Maybe.withDefault False
                           )

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        HasType outType ->
            case inscope of
                HasType inType ->
                    outType == inType

                HasUserType inType _ _ ->
                    inType == outType

                IsItem inType _ ->
                    outType == inType

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        And s1 s2 ->
            containsScope allT allH inscope s1 && containsScope allT allH inscope s2

        Or s1 s2 ->
            containsScope allT allH inscope s1 || containsScope allT allH inscope s2

        Not s ->
            not (containsScope allT allH inscope s)

        _ ->
            False


getUpper : Dict String (Typed a) -> Dict String (Hierarchic b) -> Scope -> Maybe Scope
getUpper allT allH scope =
    case scope of
        IsItem (Type.HType ht) uuid ->
            H.find allH uuid
                |> Maybe.andThen .parent
                |> (\mpuuid ->
                        case mpuuid of
                            Just puuid ->
                                Just <| HasUserType (Type.HType ht) ht puuid

                            Nothing ->
                                Just <| HasUserType (Type.HType ht) ht uuid
                   )

        IsItem (Type.TType tt) uuid ->
            T.find allT uuid
                |> Maybe.map .type_
                |> (\mpuuid ->
                        case mpuuid of
                            Just puuid ->
                                Just <| HasUserType (Type.TType tt) (TType.toHierarchic tt) puuid

                            Nothing ->
                                Just <| HasType (Type.TType tt)
                   )

        HasType _ ->
            Nothing

        HasUserType t parentType parentUuid ->
            H.find allH parentUuid
                |> Maybe.map
                    (\parent ->
                        case Maybe.andThen (H.find allH) parent.parent of
                            Just p ->
                                Just (HasUserType t p.what p.uuid)

                            Nothing ->
                                Just (HasType t)
                    )
                |> Maybe.withDefault (Just (HasType t))

        And s1 s2 ->
            let
                up1 =
                    getUpper allT allH s1

                up2 =
                    getUpper allT allH s2
            in
            Maybe.map2 And up1 up2 |> otherwise up1 |> otherwise up2

        Or s1 s2 ->
            Maybe.map2 Or (getUpper allT allH s1) (getUpper allT allH s2)

        Not _ ->
            Nothing

        Identified _ ->
            Nothing

        Empty ->
            Nothing
