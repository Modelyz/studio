module Scope.Scope exposing (Scope(..), anything, compare, decoder, empty, encode, or, toString, toType)

import Ident.Identification as Identification exposing (Identification)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type
    Scope
    -- a scope is the definition of a set of items:
    -- Either an empty set:
    = Empty
      -- or the set of all sets
    | Anything
      -- A set with a single item of type Type:
    | IsItem Type Uuid
      -- the set of items of type Type whose type_ or parent is child of a precise user type
    | HasUserType Type Uuid
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


empty : Scope
empty =
    Empty


anything : Scope
anything =
    Anything


or : Scope -> Scope -> Scope
or s1 s2 =
    Or s1 s2


encode : Scope -> Encode.Value
encode scope =
    case scope of
        HasType t ->
            Encode.object [ ( "IsType", Type.encode t ) ]

        HasUserType t uuid ->
            Encode.object [ ( "HasUserType", Encode.object [ ( "what", Type.encode t ), ( "typeUuid", Uuid.encode uuid ) ] ) ]

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

        Anything ->
            Encode.string "Anything"

        Empty ->
            Encode.string "Nothing"


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
        , Decode.map2 HasUserType
            (Decode.at [ "HasUserType", "what" ] Type.decoder)
            (Decode.at [ "HasUserType", "typeUuid" ] Uuid.decoder)
        , Decode.field "And" (Decode.lazy (\_ -> pairDecoder And "And"))
        , Decode.field "Or" (Decode.lazy (\_ -> pairDecoder Or "Or"))
        , Decode.map Not (Decode.field "Not" (Decode.lazy (\_ -> decoder)))
        , Decode.map Identified (Decode.field "Identified" Identification.decoder)
        , Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "Nothing" ->
                            Decode.succeed Empty

                        "Anything" ->
                            Decode.succeed Anything

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
            "Nothing"

        Anything ->
            "Anything"

        IsItem t uuid ->
            Type.toString t ++ "(uuid=" ++ Uuid.toString uuid ++ ")"

        HasType t ->
            Type.toString t

        HasUserType t parentUuid ->
            Type.toString t ++ "(typeUuid=" ++ Uuid.toString parentUuid ++ ")"

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


toType : Scope -> Maybe Type
toType scope =
    case scope of
        Empty ->
            Nothing

        Anything ->
            -- FIXME this is inconsistent
            Nothing

        IsItem t _ ->
            Just t

        HasType t ->
            Just t

        HasUserType t _ ->
            Just t

        Identified _ ->
            Nothing

        And s1 s2 ->
            -- FIXME this is inconsistent
            Nothing

        Or s1 s2 ->
            -- FIXME this is inconsistent
            Nothing

        Not s ->
            -- FIXME this is inconsistent
            Nothing
