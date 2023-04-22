module Configuration.Zone.Fragment exposing (Fragment(..), decoder, encode, toDesc, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Scope exposing (Scope(..))
import Time exposing (Month(..))


type alias Separator =
    String


type alias Name =
    String


type
    Fragment
    -- TODO: add Types to be able to display the type in the zone
    = IdentifierName Name
    | GroupIdentifierName Scope Name {- to display the name of a group of the entity -}
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

        GroupIdentifierName scope name ->
            name ++ " of " ++ Scope.toString scope

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

        GroupIdentifierName _ _ ->
            "Identifier of a Group"

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
                [ ( "type", Encode.string "IdentifierName" ), ( "name", Encode.string name ) ]

        GroupIdentifierName group name ->
            Encode.object
                [ ( "type", Encode.string "GroupIdentifierName" )
                , ( "scope", Scope.encode group )
                , ( "name", Encode.string name )
                ]

        Parent ->
            Encode.object [ ( "type", Encode.string "Parent" ) ]

        Fixed string ->
            Encode.object [ ( "type", Encode.string "Fixed" ), ( "string", Encode.string string ) ]

        Quantity ->
            Encode.object [ ( "type", Encode.string "Quantity" ) ]

        Flow ->
            Encode.object [ ( "type", Encode.string "Flow" ) ]

        Provider ->
            Encode.object [ ( "type", Encode.string "Provider" ) ]

        Receiver ->
            Encode.object [ ( "type", Encode.string "Receiver" ) ]

        EventList qSeparator rSeparator ->
            Encode.object
                [ ( "type", Encode.string "EventList" )
                , ( "qSep", Encode.string qSeparator )
                , ( "rSep", Encode.string rSeparator )
                ]


decoder : Decoder Fragment
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
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

                    "Fixed" ->
                        Decode.map Fixed (Decode.field "string" Decode.string)

                    "IdentifierName" ->
                        Decode.map IdentifierName (Decode.field "name" Decode.string)

                    "GroupIdentifierName" ->
                        Decode.map2 GroupIdentifierName (Decode.field "scope" Scope.decoder) (Decode.field "name" Decode.string)

                    "EventList" ->
                        Decode.map2 EventList (Decode.field "qSep" Decode.string) (Decode.field "rSep" Decode.string)

                    _ ->
                        Decode.fail ("Unknown fragment: " ++ t)
            )
