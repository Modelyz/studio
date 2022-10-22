module Zone.Fragment exposing (Fragment(..), decoder, display, encode, toDesc, toString)

import Dict exposing (Dict)
import Ident.Identifier as Identifier exposing (Identifier)
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

        Fixed _ ->
            ""


toValue : Dict String Identifier -> Fragment -> String
toValue identifiers f =
    case f of
        IdentifierName name ->
            Identifier.select name identifiers |> Maybe.map Identifier.toValue |> Maybe.withDefault "(no identifiers)"

        Fixed string ->
            string


display : Dict String Identifier -> List Fragment -> String
display identifiers fragments =
    -- display the fragments corresponding to identifiers to construct the zone
    Debug.log "fragments" (fragments |> List.map (toValue identifiers) |> String.concat)


toDesc : Fragment -> String
toDesc f =
    case f of
        IdentifierName _ ->
            "(Identifier)"

        Fixed _ ->
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
