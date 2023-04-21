module Configuration.Zone.Fragment exposing (Fragment(..), decoder, encode, toDesc, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Scope exposing (Scope(..))


type alias Separator =
    String


type alias Name =
    String


type
    Fragment
    -- TODO: add Types to be able to display the type in the zone
    = IdentifierName Name
    | GroupIdentifierName Name
    | Parent
    | Fixed String
    | Quantity
    | Flow
    | Provider
    | Receiver
    | EventList Separator Separator


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
            "Fixed"

        Quantity ->
            "Quantity"

        Flow ->
            "Flow"

        Provider ->
            "Provider"

        Receiver ->
            "Receiver"

        EventList _ _ ->
            "EventList"


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

        EventList _ _ ->
            "Display the list of partial Events, separated by a configurable separator."


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

        EventList qSeparator rSeparator ->
            Encode.object
                [ ( "type", Encode.string "EventList" )
                , ( "qSep", Encode.string qSeparator )
                , ( "rSep", Encode.string rSeparator )
                ]


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
        , Decode.map3 (\_ q r -> EventList q r)
            (Decode.field "type" Decode.string)
            (Decode.field "qSep" Decode.string)
            (Decode.field "rSep" Decode.string)
        ]
