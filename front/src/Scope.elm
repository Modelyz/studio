module Scope exposing (Scope(..), anything, compare, decoder, empty, encode, or, toString, toType)

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
        Empty ->
            Encode.object [ ( "scope", Encode.string "Nothing" ) ]

        Anything ->
            Encode.object [ ( "scope", Encode.string "Anything" ) ]

        IsItem t u ->
            Encode.object [ ( "scope", Encode.string "IsItem" ), ( "value", Encode.object [ ( "what", Type.encode t ), ( "uuid", Uuid.encode u ) ] ) ]

        HasUserType t u ->
            Encode.object [ ( "scope", Encode.string "HasUserType" ), ( "value", Encode.object [ ( "what", Type.encode t ), ( "uuid", Uuid.encode u ) ] ) ]

        HasType t ->
            Encode.object [ ( "scope", Encode.string "HasType" ), ( "value", Type.encode t ) ]

        And s1 s2 ->
            Encode.object [ ( "scope", Encode.string "And" ), ( "value", Encode.object [ ( "scope1", encode s1 ), ( "scope2", encode s2 ) ] ) ]

        Or s1 s2 ->
            Encode.object [ ( "scope", Encode.string "Or" ), ( "value", Encode.object [ ( "scope1", encode s1 ), ( "scope2", encode s2 ) ] ) ]

        Not s ->
            Encode.object [ ( "scope", Encode.string "Not" ), ( "value", encode s ) ]

        Identified id ->
            Encode.object [ ( "scope", Encode.string "Identified" ), ( "value", Identification.encode id ) ]


decoder : Decoder Scope
decoder =
    Decode.field "scope" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Empty" ->
                        Decode.succeed Empty

                    "Anything" ->
                        Decode.succeed Anything

                    "IsItem" ->
                        Decode.map2 IsItem
                            (Decode.field "type" Type.decoder)
                            (Decode.field "uuid" Uuid.decoder)

                    "HasUserType" ->
                        Decode.map2 HasUserType
                            (Decode.field "type" Type.decoder)
                            (Decode.field "uuid" Uuid.decoder)

                    "HasType" ->
                        Decode.map HasType (Decode.field "value" Type.decoder)

                    "And" ->
                        Decode.field "value" (Decode.map2 And (Decode.field "scope1" (Decode.lazy (\_ -> decoder))) (Decode.field "scope2" (Decode.lazy (\_ -> decoder))))

                    "Or" ->
                        Decode.field "value" (Decode.map2 Or (Decode.field "scope1" (Decode.lazy (\_ -> decoder))) (Decode.field "scope2" (Decode.lazy (\_ -> decoder))))

                    "Not" ->
                        Decode.field "value" (Decode.lazy (\_ -> decoder))

                    "Identified" ->
                        Decode.map Identified (Decode.field "value" Identification.decoder)

                    _ ->
                        Decode.fail "Invalid scope definition"
            )


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
            -- TODO this is inconsistent. Try to remove this function
            Nothing

        IsItem t _ ->
            Just t

        HasType t ->
            Just t

        HasUserType t _ ->
            Just t

        Identified _ ->
            Nothing

        And _ _ ->
            -- TODO this is inconsistent. Try to remove this function
            Nothing

        Or _ _ ->
            -- TODO this is inconsistent. Try to remove this function
            Nothing

        Not _ ->
            -- TODO this is inconsistent. Try to remove this function
            Nothing
