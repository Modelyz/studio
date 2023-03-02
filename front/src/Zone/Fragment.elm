module Zone.Fragment exposing (Fragment(..), decoder, encode, toDesc, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Scope exposing (Scope(..))


type
    Fragment
    -- TODO: add Types to be able to display the type in the zone
    = IdentifierName String
    | GroupIdentifierName String
    | Parent
    | Fixed String
    | Quantity
    | Flow
    | Provider
    | Receiver


toString : Fragment -> String
toString fragment =
    case fragment of
        IdentifierName name ->
            name

        GroupIdentifierName name ->
            name

        Parent ->
            "Parent"

        Fixed _ ->
            "Fixed:"

        Quantity ->
            "Quantity"

        Flow ->
            "Flow"

        Provider ->
            "Provider"

        Receiver ->
            "Receiver"


toDesc : Fragment -> String
toDesc fragment =
    case fragment of
        IdentifierName _ ->
            "Identifier"

        GroupIdentifierName _ ->
            "Group Identifier"

        Parent ->
            "Display the zone configuration of the Parent"

        Fixed _ ->
            "Always display a predefined string"

        Quantity ->
            "Display the quantity of the event"

        Flow ->
            "Display the Resource or ResourceType of the Event or Commitment"

        Provider ->
            "Display the provider of the Event or Commitment"

        Receiver ->
            "Display the receiver of the Event or Commitment"


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

        Quantity ->
            Encode.string "Quantity"

        Flow ->
            Encode.string "Flow"

        Provider ->
            Encode.string "Provider"

        Receiver ->
            Encode.string "Receiver"


decoder : Decoder Fragment
decoder =
    Decode.oneOf
        [ Decode.map IdentifierName <| Decode.field "IdentifierName" Decode.string
        , Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "Parent" ->
                            Decode.succeed Parent

                        "Quantity" ->
                            Decode.succeed Quantity

                        "Flow" ->
                            Decode.succeed Flow

                        "Provider" ->
                            Decode.succeed Provider

                        "Receiver" ->
                            Decode.succeed Receiver

                        _ ->
                            Decode.fail ("Unknown fragment: " ++ s)
                )
        , Decode.map GroupIdentifierName <| Decode.field "GroupIdentifierName" Decode.string
        , Decode.map Fixed <| Decode.field "Fixed" Decode.string
        ]
