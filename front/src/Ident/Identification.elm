module Ident.Identification exposing (Identification, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Identification
    = StartsWith String
    | EndsWith String
    | Contains String



--    | Match Pattern -- ??


encode : Identification -> Encode.Value
encode id =
    case id of
        StartsWith str ->
            Encode.object [ ( "type", Encode.string "StartsWith" ), ( "value", Encode.object [ ( "value", Encode.string str ) ] ) ]

        EndsWith str ->
            Encode.object [ ( "type", Encode.string "EndsWith" ), ( "value", Encode.object [ ( "value", Encode.string str ) ] ) ]

        Contains str ->
            Encode.object [ ( "type", Encode.string "Contains" ), ( "value", Encode.object [ ( "value", Encode.string str ) ] ) ]


decoder : Decoder Identification
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "StartsWith" ->
                        Decode.map StartsWith (Decode.field "value" Decode.string)

                    "EndsWith" ->
                        Decode.map EndsWith (Decode.field "value" Decode.string)

                    "Contains" ->
                        Decode.map Contains (Decode.field "value" Decode.string)

                    _ ->
                        Decode.fail "Invalid Identification"
            )
