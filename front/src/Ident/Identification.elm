module Ident.Identification exposing (Identification, decoder, encode, view)

import Element exposing (..)
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
            Encode.object [ ( "StartsWith", Encode.string str ) ]

        EndsWith str ->
            Encode.object [ ( "EndsWith", Encode.string str ) ]

        Contains str ->
            Encode.object [ ( "Contains", Encode.string str ) ]


decoder : Decoder Identification
decoder =
    Decode.oneOf
        [ Decode.map StartsWith (Decode.field "StartsWith" Decode.string)
        , Decode.map EndsWith (Decode.field "EndsWith" Decode.string)
        , Decode.map Contains (Decode.field "Contains" Decode.string)
        ]


toString : Identification -> String
toString i =
    case i of
        StartsWith str ->
            "Starts with \"" ++ str ++ "\""

        EndsWith str ->
            "Ends with with \"" ++ str ++ "\""

        Contains str ->
            "Contains \"" ++ str ++ "\""


view : Identification -> Element msg
view i =
    row [] [ text <| toString i ]
