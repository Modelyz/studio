module Scope.Scope exposing (Scope(..), compare, containsItem, containsScope, decoder, encode, getUpperList, mainHType, mainTType, toString)

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
    -- Either an empty set
    = Empty
      -- A set with a single item
    | IsItem Type Uuid
      -- The set of items with a specific concrete typz
    | HasType Type
      -- the set of items of type Type whose type or parent is child of a user type uuid
    | HasUserType Type Uuid
      -- The union of two sets
    | And Scope Scope -- entities of both groups
      -- An alternative between two sets
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

        HasUserType t uuid ->
            Encode.object [ ( "HasUserType", Encode.object [ ( "type", Type.encode t ), ( "uuid", Uuid.encode uuid ) ] ) ]

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
        , Decode.map2 HasUserType (Decode.at [ "HasUserType", "type" ] Type.decoder) (Decode.at [ "HasUserType", "uuid" ] Uuid.decoder)
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
        IsItem t uuid ->
            T.find allT uuid |> Maybe.map .type_ |> Maybe.map (HasUserType t)

        HasType _ ->
            Nothing

        HasUserType t uuid ->
            -- uuid here is always a hierarchic type
            H.find allH uuid
                |> Maybe.map
                    (\h ->
                        case h.parent of
                            Nothing ->
                                Just (HasType t)

                            Just x ->
                                Just (HasUserType t x)
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
containsScope allT allH ins out =
    let
        inscope =
            Debug.log "inscope" ins

        outscope =
            Debug.log "outscope" out
    in
    -- definitely need review
    Debug.log "RESULT" <|
        case outscope of
            Empty ->
                False

            IsItem outType outUuid ->
                case outType of
                    Type.TType _ ->
                        case inscope of
                            IsItem inType inUuid ->
                                outType == inType && outUuid == inUuid

                            And s1 s2 ->
                                containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                            Or s1 s2 ->
                                containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                            _ ->
                                False

                    Type.HType _ ->
                        case inscope of
                            IsItem inType inUuid ->
                                outType == inType && outUuid == inUuid

                            And s1 s2 ->
                                containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                            Or s1 s2 ->
                                containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                            _ ->
                                False

            HasType outType ->
                case inscope of
                    IsItem inType _ ->
                        outType == inType

                    HasType inType ->
                        outType == inType

                    HasUserType inType _ ->
                        inType == outType

                    And s1 s2 ->
                        containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                    Or s1 s2 ->
                        containsScope allT allH s1 outscope && containsScope allT allH s2 outscope

                    _ ->
                        False

            HasUserType outType outUuid ->
                case inscope of
                    IsItem inType inUuid ->
                        (inType == outType)
                            && (Maybe.map3 T.isAscendantOf (T.find allT inUuid) (Just allH) (H.find allH outUuid)
                                    |> Maybe.withDefault False
                               )

                    HasUserType inType inUuid ->
                        (inType == outType)
                            && (Maybe.map3 H.isAscendantOf (H.find allH inUuid) (Just allH) (H.find allH outUuid)
                                    |> Maybe.withDefault False
                               )

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


containsItem : Scope -> Item a -> Bool
containsItem scope item =
    case scope of
        Empty ->
            False

        IsItem _ uuid ->
            item.uuid == uuid

        HasType t ->
            -- TODO a non-alias type would allow to get rid of "what"
            item.what == t

        HasUserType _ _ ->
            -- ancestor search is in functions below
            False

        Identified _ ->
            -- TODO
            False

        And s1 s2 ->
            containsItem s1 item && containsItem s2 item

        Or s1 s2 ->
            containsItem s1 item || containsItem s2 item

        Not s ->
            not <| containsItem s item


toString : Scope -> String
toString scope =
    -- for technical use (compare)
    case scope of
        Empty ->
            "Empty"

        IsItem t uuid ->
            Type.toString t ++ "(uuid=" ++ Uuid.toString uuid ++ ")"

        HasType t ->
            Type.toString t

        HasUserType t tuid ->
            Type.toString t ++ "(parent=" ++ Uuid.toString tuid ++ ")"

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

        HasUserType t _ ->
            case t of
                Type.TType tt ->
                    Just tt

                Type.HType ht ->
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

        HasUserType t _ ->
            case t of
                Type.TType tt ->
                    Just (TType.toHierarchic tt)

                Type.HType ht ->
                    Just ht

        And s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        Or s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        _ ->
            Nothing
