module Scope.Scope exposing (Scope(..), compare, containsItem, containsScope, decoder, encode, fromHierarchic, fromTyped, getUpperList, mainHType, mainTType, toString)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identification as Identification exposing (Identification)
import Item.Item as Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Tuple
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed as T exposing (Typed)


type
    Scope
    -- a scope is the definition of a set of items:
    -- Either an empty set
    = Empty
      -- A set with a single typed item
    | TItem Uuid
      -- A set with a single hierarchic item
    | HItem Uuid
      -- The union of two sets
    | And Scope Scope -- entities of both groups
      -- An alternative between two sets
    | Or Scope Scope -- entities of either group
      -- Everything but the set
    | Not Scope -- entities not in the group
      -- The set of items with a specific concrete typz
    | IsType Type
      -- the set of items whose type or parent is child of a user type
    | HasUserType Uuid
      -- the set of items with a specific identification
    | Identified Identification -- entities identified somehow


encode : Scope -> Encode.Value
encode scope =
    case scope of
        TItem uuid ->
            Encode.object [ ( "TItem", Uuid.encode uuid ) ]

        HItem uuid ->
            Encode.object [ ( "HItem", Uuid.encode uuid ) ]

        IsType string ->
            Encode.object [ ( "IsType", Type.encode string ) ]

        HasUserType uuid ->
            Encode.object [ ( "HasUserType", Uuid.encode uuid ) ]

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
        [ Decode.field "TItem" Uuid.decoder |> Decode.map TItem
        , Decode.field "HItem" Uuid.decoder |> Decode.map HItem
        , Decode.field "And" (Decode.lazy (\_ -> pairDecoder And "And"))
        , Decode.field "Or" (Decode.lazy (\_ -> pairDecoder Or "Or"))
        , Decode.map Not (Decode.field "Not" (Decode.lazy (\_ -> decoder)))
        , Decode.map IsType (Decode.field "IsType" Type.decoder)
        , Decode.map HasUserType (Decode.field "HasUserType" Uuid.decoder)
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


fromTyped : Typed a -> Scope
fromTyped t =
    And (IsType t.what) (HasUserType t.type_)


fromHierarchic : Hierarchic a -> Scope
fromHierarchic h =
    h.parent |> Maybe.map (\p -> And (IsType h.what) (HasUserType p)) |> Maybe.withDefault (IsType h.what)


getUpper : Dict String (Typed a) -> Dict String (Hierarchic b) -> Scope -> Maybe Scope
getUpper allT allH scope =
    case scope of
        TItem uuid ->
            T.find allT uuid |> Maybe.map .type_ |> Maybe.map HasUserType

        HItem uuid ->
            H.find allH uuid |> Maybe.andThen .parent |> Maybe.map HasUserType

        IsType string ->
            Nothing

        HasUserType uuid ->
            H.find allH uuid |> Maybe.map (\p -> p.parent |> Maybe.map HasUserType >> Maybe.withDefault (IsType p.what))

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

        Not s ->
            Nothing

        Identified id ->
            Nothing

        Empty ->
            Nothing


getUpperList : Dict String (Typed a) -> Dict String (Hierarchic b) -> Scope -> List Scope -> List Scope
getUpperList allT allH scope currentList =
    getUpper allT allH scope
        |> Maybe.map (\upperScope -> getUpperList allT allH upperScope currentList)
        |> Maybe.withDefault (scope :: currentList)


containsScope : Dict String (Typed a) -> Dict String (Hierarchic b) -> Scope -> Scope -> Bool
containsScope allT allH outscope inscope =
    let
        emptyT =
            Dict.empty

        emptyH =
            Dict.empty
    in
    case outscope of
        Empty ->
            False

        TItem outItem ->
            case inscope of
                TItem inItem ->
                    outItem == inItem

                And s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

                Or s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

                _ ->
                    False

        HItem outItem ->
            case inscope of
                HItem inItem ->
                    Maybe.map3 H.isAscendantOf (H.find allH inItem) (Just allH) (H.find allH outItem) |> Maybe.withDefault False

                And s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

                Or s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

                _ ->
                    False

        IsType t ->
            case inscope of
                TItem uuid ->
                    Maybe.map (.what >> (==) t) (T.find allT uuid) |> Maybe.withDefault False

                HItem uuid ->
                    Maybe.map (.what >> (==) t) (H.find allH uuid) |> Maybe.withDefault False

                IsType u ->
                    t == u

                HasUserType tuid ->
                    Maybe.map (.what >> (==) t) (H.find allH tuid) |> Maybe.withDefault False

                And s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

                Or s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

                _ ->
                    False

        HasUserType tuid ->
            case inscope of
                TItem uuid ->
                    Maybe.map3 T.isAscendantOf (T.find allT uuid) (Just allH) (H.find allH tuid) |> Maybe.withDefault False

                HItem uuid ->
                    Maybe.map2 (\t u -> H.isAscendantOf u allH t) (H.find allH tuid) (H.find allH uuid) |> Maybe.withDefault False

                HasUserType tuid2 ->
                    Maybe.map2 (\t u -> H.isAscendantOf u allH t) (H.find allH tuid) (H.find allH tuid2) |> Maybe.withDefault False

                And s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

                Or s1 s2 ->
                    containsScope emptyT emptyH s1 outscope && containsScope emptyT emptyH s2 outscope

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

        TItem uuid ->
            item.uuid == uuid

        HItem uuid ->
            item.uuid == uuid

        IsType t ->
            item.what == t

        HasUserType tuid ->
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


containsTyped : Dict String (Typed a) -> Dict String (Hierarchic (Item a)) -> Scope -> Typed (Item a) -> Bool
containsTyped allT allH scope item =
    case scope of
        HasUserType tuid ->
            Maybe.map (T.isAscendantOf item allH) (H.find allH tuid)
                |> Maybe.withDefault False

        _ ->
            containsItem scope item


containsHierarchic : Dict String (Hierarchic a) -> Dict String (Hierarchic (Item a)) -> Scope -> Hierarchic (Item a) -> Bool
containsHierarchic allT allH scope item =
    case scope of
        HasUserType tuid ->
            Maybe.map (H.isAscendantOf item allH) (H.find allH tuid)
                |> Maybe.withDefault False

        _ ->
            containsItem scope item


toString : Scope -> String
toString scope =
    case scope of
        Empty ->
            "Empty"

        TItem uuid ->
            "Typed Item with uuid=" ++ Uuid.toString uuid

        HItem uuid ->
            "Hierarchic Item with uuid=" ++ Uuid.toString uuid

        IsType t ->
            "IsType " ++ Type.toString t

        HasUserType tuid ->
            "HasUserType " ++ Uuid.toString tuid

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


otherwise : Maybe a -> Maybe a -> Maybe a
otherwise x y =
    case x of
        Just z ->
            Just z

        Nothing ->
            y


mainTType : Scope -> Maybe TType.Type
mainTType scope =
    case scope of
        IsType t ->
            case t of
                Type.TType tt ->
                    Just tt

                Type.HType ht ->
                    Nothing

        And s1 s2 ->
            otherwise (mainTType s1) (mainTType s2)

        Or s1 s2 ->
            otherwise (mainTType s1) (mainTType s2)

        _ ->
            Nothing


mainHType : Scope -> Maybe HType.Type
mainHType scope =
    case scope of
        IsType t ->
            case t of
                Type.TType tt ->
                    Nothing

                Type.HType ht ->
                    Just ht

        And s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        Or s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        _ ->
            Nothing
