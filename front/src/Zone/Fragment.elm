module Zone.Fragment exposing (Fragment(..), decoder, display, encode, toDesc, toString)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier as Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type Fragment
    = IdentifierName String
    | GroupIdentifierName String
    | Fixed String


toString : Fragment -> String
toString fragment =
    case fragment of
        IdentifierName name ->
            name

        GroupIdentifierName name ->
            name

        Fixed s ->
            s


toValue : Dict String Identifier -> Dict String Identifier -> Fragment -> String
toValue identifiers groupids fragment =
    case fragment of
        IdentifierName name ->
            Identifier.select name identifiers |> List.map Identifier.toValue |> String.join ", "

        GroupIdentifierName name ->
            Identifier.select name groupids |> List.map Identifier.toValue |> String.join ", "

        Fixed string ->
            string


display : Dict String Identifier -> Dict String Identifier -> List Fragment -> String
display identifiers groupids fragments =
    -- display the fragments corresponding to identifiers to construct the zone
    fragments |> List.map (toValue identifiers groupids) |> String.concat


toDesc : Fragment -> String
toDesc fragment =
    case fragment of
        IdentifierName _ ->
            "Identifier"

        GroupIdentifierName _ ->
            "Group Identifier"

        Fixed _ ->
            "Fixed string"


encode : Fragment -> Encode.Value
encode fragment =
    case fragment of
        IdentifierName name ->
            Encode.object
                [ ( "IdentifierName", Encode.string name ) ]

        GroupIdentifierName name ->
            Encode.object
                [ ( "GroupIdentifierName", Encode.string name ) ]

        Fixed string ->
            Encode.object
                [ ( "Fixed", Encode.string string ) ]


decoder : Decoder Fragment
decoder =
    Decode.oneOf
        [ Decode.field "IdentifierName" Decode.string |> Decode.map IdentifierName
        , Decode.field "GroupIdentifierName" Decode.string |> Decode.map GroupIdentifierName
        , Decode.field "Fixed" Decode.string |> Decode.map Fixed
        ]
