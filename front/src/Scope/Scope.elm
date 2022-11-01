module Scope.Scope exposing (Scope(..), compare, containsHierarchic, containsScope, containsTyped, decoder, encode, getUpperList, mainHType, mainTType, toString)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identification as Identification exposing (Identification)
import Item.Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed as T exposing (Typed)
import Util exposing (otherwise)


type
    Scope
    -- a scope is the definition of a set of items:
    -- Either an empty set:
    = Empty
      -- A set with a single item of type Type:
    | IsItem Type Uuid
      -- the set of items of type Type whose type_ or parent is child of a user type of type Type and uuid Uuid:
    | HasUserType HType.Type Uuid
      -- The set of items with a specific concrete type:
    | HasType Type
      -- TODO : need to rethink what is below. Seems not relevant for Ident and Value. Can an entity be of several type?? Or is it useful for search? Maybe we need to implement a search expression like the one in Value and which is different from the scope?
      -- The union of two sets
    | And Scope Scope -- entities of both groups
      -- An alternative between two sets.
    | Or Scope Scope -- entities of either group
      -- Everything but the set
    | Not Scope -- entities not in the group
      -- the set of items with a specific identification
    | Identified Identification -- entities identified somehow


encode : Scope -> Encode.Value
encode scope =
    case scope of
        HasType t ->
            Encode.object [ ( "IsType", Type.encode t ) ]

        HasUserType ht uuid ->
            Encode.object [ ( "HasUserType", Encode.object [ ( "type", HType.encode ht ), ( "uuid", Uuid.encode uuid ) ] ) ]

        IsItem t uuid ->
            Encode.object [ ( "IsItem", Encode.object [ ( "type", Type.encode t ), ( "uuid", Uuid.encode uuid ) ] ) ]

        And s1 s2 ->
            Encode.object [ ( "And", Encode.list encode [ s1, s2 ] ) ]

        Or s1 s2 ->
            Encode.object [ ( "Or", Encode.list encode [ s1, s2 ] ) ]

        Not s ->
            Encode.object [ ( "Not", encode s ) ]

        Identified id ->
            Encode.object [ ( "Identified", Identification.encode id ) ]

        Empty ->
            Encode.string "Empty"


pairDecoder : (Scope -> Scope -> Scope) -> String -> Decoder Scope
pairDecoder constructor str =
    Decode.list decoder
        |> Decode.andThen
            (\l ->
                Maybe.map2 constructor
                    (List.head l)
                    (List.tail l
                        |> Maybe.andThen List.head
                    )
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail <| "The " ++ str ++ " operator should contain exactly two scopes")
            )


decoder : Decoder Scope
decoder =
    Decode.oneOf
        [ Decode.map HasType (Decode.field "IsType" Type.decoder)
        , Decode.map2 IsItem (Decode.at [ "IsItem", "type" ] Type.decoder) (Decode.at [ "IsItem", "uuid" ] Uuid.decoder)
        , Decode.map2 HasUserType (Decode.at [ "HasUserType", "type" ] HType.decoder) (Decode.at [ "HasUserType", "uuid" ] Uuid.decoder)
        , Decode.field "And" (Decode.lazy (\_ -> pairDecoder And "And"))
        , Decode.field "Or" (Decode.lazy (\_ -> pairDecoder Or "Or"))
        , Decode.map Not (Decode.field "Not" (Decode.lazy (\_ -> decoder)))
        , Decode.map Identified (Decode.field "Identified" Identification.decoder)
        , Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "Empty" ->
                            Decode.succeed Empty

                        _ ->
                            Decode.fail "Invalid scope definition"
                )
        ]


getUpper : Dict String (Typed a) -> Dict String (Hierarchic b) -> Scope -> Maybe Scope
getUpper allT allH scope =
    case scope of
        IsItem (Type.HType ht) uuid ->
            H.find allH uuid
                |> Maybe.andThen .parent
                |> (\mpuuid ->
                        case mpuuid of
                            Just puuid ->
                                Just <| HasUserType ht puuid

                            Nothing ->
                                Just <| HasUserType ht uuid
                   )

        IsItem (Type.TType tt) uuid ->
            T.find allT uuid
                |> Maybe.map .type_
                |> (\mpuuid ->
                        case mpuuid of
                            Just puuid ->
                                Just <| HasUserType (TType.toHierarchic tt) puuid

                            Nothing ->
                                Just <| HasType (Type.TType tt)
                   )

        HasType _ ->
            Nothing

        HasUserType ht uuid ->
            -- uuid here is always a hierarchic type
            H.find allH uuid
                |> Maybe.map
                    (\h ->
                        case h.parent of
                            Nothing ->
                                Just (HasType (Type.HType ht))

                            Just x ->
                                Just (HasUserType ht x)
                    )
                |> Maybe.withDefault (Just (HasType (Type.HType ht)))

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
    -- definitely needs review
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

        IsItem (Type.HType outHType) outUuid ->
            case inscope of
                IsItem (Type.HType inType) inUuid ->
                    outHType == inType && outUuid == inUuid

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        HasUserType outHType outUuid ->
            case inscope of
                IsItem (Type.TType inTType) inUuid ->
                    (outHType == TType.toHierarchic inTType)
                        && (Maybe.map3 T.isAscendantOf (T.find allT inUuid) (Just allH) (H.find allH outUuid)
                                |> Maybe.withDefault False
                           )

                IsItem (Type.HType inHType) inUuid ->
                    (outHType == inHType)
                        && (Maybe.map3 T.isAscendantOf (T.find allT inUuid) (Just allH) (H.find allH outUuid)
                                |> Maybe.withDefault False
                           )

                HasUserType inHType inUuid ->
                    (outHType == inHType)
                        && (Maybe.map3 H.isAscendantOf (H.find allH inUuid) (Just allH) (H.find allH outUuid)
                                |> Maybe.withDefault False
                           )

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        HasType (Type.HType outHType) ->
            case inscope of
                IsItem (Type.HType inHType) _ ->
                    outHType == inHType

                HasType (Type.HType inHType) ->
                    outHType == inHType

                HasUserType inHType _ ->
                    inHType == outHType

                And s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                Or s1 s2 ->
                    containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                _ ->
                    False

        HasType (Type.TType outTType) ->
            case inscope of
                HasType (Type.TType inTType) ->
                    outTType == inTType

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


containsTyped : Scope -> Typed a -> Bool
containsTyped scope t =
    case scope of
        Empty ->
            False

        IsItem _ uuid ->
            t.uuid == uuid

        HasType (Type.HType ht) ->
            -- TODO a non-alias type would allow to get rid of "what"
            t.what == TType.fromHierarchic ht

        HasType (Type.TType tt) ->
            -- TODO a non-alias type would allow to get rid of "what"
            t.what == tt

        HasUserType _ _ ->
            -- ancestor search is in functions below
            False

        Identified _ ->
            -- TODO
            False

        And s1 s2 ->
            containsTyped s1 t && containsTyped s2 t

        Or s1 s2 ->
            containsTyped s1 t || containsTyped s2 t

        Not s ->
            not <| containsTyped s t


containsHierarchic : Scope -> Hierarchic a -> Bool
containsHierarchic scope h =
    case scope of
        Empty ->
            False

        IsItem _ uuid ->
            h.uuid == uuid

        HasType (Type.TType tt) ->
            -- TODO a non-alias type would allow to get rid of "what"
            h.what == TType.toHierarchic tt

        HasType (Type.HType ht) ->
            -- TODO a non-alias type would allow to get rid of "what"
            h.what == ht

        HasUserType _ _ ->
            -- ancestor search is in functions below
            False

        Identified _ ->
            -- TODO
            False

        And s1 s2 ->
            containsHierarchic s1 h && containsHierarchic s2 h

        Or s1 s2 ->
            containsHierarchic s1 h || containsHierarchic s2 h

        Not s ->
            not <| containsHierarchic s h


toString : Scope -> String
toString scope =
    -- for technical use (like compare)
    -- Avoid putting "/" in the String because it will break the ValueType URL
    case scope of
        Empty ->
            "Empty"

        IsItem t uuid ->
            Type.toString t ++ "(uuid=" ++ Uuid.toString uuid ++ ")"

        HasType t ->
            Type.toString t

        HasUserType ht tuid ->
            HType.toString ht ++ "(parent=" ++ Uuid.toString tuid ++ ")"

        Identified _ ->
            "Identified"

        And s1 s2 ->
            "(" ++ toString s1 ++ ") And (" ++ toString s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toString s1 ++ ") Or (" ++ toString s2 ++ ")"

        Not s ->
            "Not (" ++ toString s ++ ")"


compare : Scope -> String
compare =
    toString


mainTType : Scope -> Maybe TType.Type
mainTType scope =
    case scope of
        HasType t ->
            case t of
                Type.TType tt ->
                    Just tt

                Type.HType ht ->
                    Just (TType.fromHierarchic ht)

        HasUserType ht _ ->
            Just (TType.fromHierarchic ht)

        And s1 s2 ->
            otherwise (mainTType s1) (mainTType s2)

        Or s1 s2 ->
            otherwise (mainTType s1) (mainTType s2)

        _ ->
            Nothing


mainHType : Scope -> Maybe HType.Type
mainHType scope =
    case scope of
        HasType t ->
            case t of
                Type.TType tt ->
                    Just (TType.toHierarchic tt)

                Type.HType ht ->
                    Just ht

        HasUserType ht _ ->
            Just ht

        And s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        Or s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        _ ->
            Nothing
