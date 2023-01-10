module Zone.Fragment exposing (Fragment(..), decoder, encode, toDesc, toString)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier as Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type
    Fragment
    -- TODO: add Types to be able to display the type in the zone
    = IdentifierName String
    | GroupIdentifierName String
    | Parent
    | Fixed String


toString : Fragment -> String
toString fragment =
    case fragment of
        IdentifierName name ->
            name

        GroupIdentifierName name ->
            name

        Parent ->
            "Parent"

        Fixed s ->
            "Fixed:"


toDesc : Fragment -> String
toDesc fragment =
    case fragment of
        IdentifierName _ ->
            "Identifier"

        GroupIdentifierName _ ->
            "Group Identifier"

        Parent ->
            "Parent"

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

        Parent ->
            Encode.string "Parent"

        Fixed string ->
            Encode.object
                [ ( "Fixed", Encode.string string ) ]


decoder : Decoder Fragment
decoder =
    Decode.oneOf
        [ Decode.map IdentifierName <| Decode.field "IdentifierName" Decode.string
        , Decode.string
            |> Decode.andThen
                (\s ->
                    if s == "Parent" then
                        Decode.succeed Parent

                    else
                        Decode.fail ("Unknown fragment: " ++ s)
                )
        , Decode.map GroupIdentifierName <| Decode.field "GroupIdentifierName" Decode.string
        , Decode.map Fixed <| Decode.field "Fixed" Decode.string
        ]
