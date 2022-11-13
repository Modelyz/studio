module Scope.Scope exposing (Scope(..), compare, decoder, encode, mainHType, mainTType, toString)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Ident.Identification as Identification exposing (Identification)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (otherwise)


type
    Scope
    -- a scope is the definition of a set of items:
    -- Either an empty set:
    = Empty
      -- A set with a single item of type Type:
    | IsItem Type Uuid
      -- the set of items of type Type whose type_ or parent is child of a precise user type
      -- TODO : convert to HasUserType Type Uuid or HasUserType HType uuid (because the HType is always related to the Type)
      -- btw, isn't the hasusertype just a htype?
    | HasUserType Type HType.Type Uuid
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

        HasUserType t ht uuid ->
            Encode.object [ ( "HasUserType", Encode.object [ ( "what", Type.encode t ), ( "type", HType.encode ht ), ( "uuid", Uuid.encode uuid ) ] ) ]

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
        , Decode.map2 IsItem
            (Decode.at [ "IsItem", "type" ] Type.decoder)
            (Decode.at [ "IsItem", "uuid" ] Uuid.decoder)
        , Decode.map3 HasUserType
            (Decode.at [ "HasUserType", "what" ] Type.decoder)
            (Decode.at [ "HasUserType", "type" ] HType.decoder)
            (Decode.at [ "HasUserType", "uuid" ] Uuid.decoder)
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


toString : Scope -> String
toString scope =
    -- for technical use (like compare)
    -- TODO remove to force use of Scope.View.toDisplay
    case scope of
        Empty ->
            "Empty"

        IsItem t uuid ->
            Type.toString t ++ "(uuid=" ++ Uuid.toString uuid ++ ")"

        HasType t ->
            Type.toString t

        HasUserType t parentType parentUuid ->
            Type.toString t ++ "(parent=" ++ Uuid.toString parentUuid ++ ")"

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
    -- TODO try to remove this function (and the one below)
    case scope of
        HasType (Type.HType ht) ->
            Just (TType.fromHierarchic ht)

        HasType (Type.TType tt) ->
            Just tt

        HasUserType (Type.HType ht) _ _ ->
            Just (TType.fromHierarchic ht)

        HasUserType (Type.TType tt) _ _ ->
            Just tt

        And s1 s2 ->
            otherwise (mainTType s1) (mainTType s2)

        Or s1 s2 ->
            otherwise (mainTType s1) (mainTType s2)

        _ ->
            Nothing


mainHType : Scope -> Maybe HType.Type
mainHType scope =
    case scope of
        HasType (Type.HType ht) ->
            Just ht

        HasType (Type.TType tt) ->
            Just (TType.toHierarchic tt)

        HasUserType (Type.HType ht) _ _ ->
            Just ht

        HasUserType (Type.TType tt) _ _ ->
            Just (TType.toHierarchic tt)

        And s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        Or s1 s2 ->
            otherwise (mainHType s1) (mainHType s2)

        _ ->
            Nothing
