module Zone.Fragment exposing (Fragment(..), decoder, encode, toDesc, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Fragment
    = IdentifierName String
    | Fixed String


toString : Fragment -> String
toString f =
    case f of
        IdentifierName name ->
            name

        Fixed string ->
            ""


toDesc : Fragment -> String
toDesc f =
    case f of
        IdentifierName name ->
            "(Identifier)"

        Fixed string ->
            "(Fixed string)"


encode : Fragment -> Encode.Value
encode f =
    case f of
        IdentifierName name ->
            Encode.object
                [ ( "IdentifierName", Encode.string name ) ]

        Fixed string ->
            Encode.object
                [ ( "Fixed", Encode.string string ) ]


decoder : Decoder Fragment
decoder =
    Decode.oneOf
        [ Decode.field "IdentifierName" Decode.string |> Decode.map IdentifierName
        , Decode.field "Fixed" Decode.string |> Decode.map Fixed
        ]