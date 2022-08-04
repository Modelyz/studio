module Group.Definition exposing (Definition(..), decoder, encode)

import Entity.Type as Type exposing (Type)
import Ident.Identification as Identification exposing (Identification)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Tuple


type
    Definition
    -- a group can be defined by its members or by the characteristics of its members
    -- TODO add Scope in a group definition?
    = Entity Uuid -- a single entity (can be a group)
    | And Definition Definition -- entities of both groups
    | Or Definition Definition -- entities of either group
    | Not Definition -- entities not in the group
    | Identified Identification -- entities identified somehow
    | Empty


encode : Definition -> Encode.Value
encode d =
    case d of
        Entity uuid ->
            Encode.object [ ( "Entity", Uuid.encode uuid ) ]

        And d1 d2 ->
            Encode.object [ ( "And", Encode.list encode [ d1, d2 ] ) ]

        Or d1 d2 ->
            Encode.object [ ( "Or", Encode.list encode [ d1, d2 ] ) ]

        Not uuid ->
            Encode.object [ ( "Not", encode uuid ) ]

        Identified id ->
            Encode.object [ ( "Identified", Identification.encode id ) ]

        Empty ->
            Encode.string "Empty"


pairDecoder : (Definition -> Definition -> Definition) -> String -> Decoder Definition
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
                    |> Maybe.withDefault (Decode.fail <| "The " ++ str ++ " operator should contain exactly two groups")
            )


decoder : Decoder Definition
decoder =
    Decode.oneOf
        [ Decode.field "Entity" Uuid.decoder |> Decode.map Entity
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
                            Decode.fail "Invalid group definition"
                )
        ]
