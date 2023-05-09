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
    = IdentifierName { name : Name }
    | GroupIdentifierName { scope : Scope, name : Name } {- to display the name of a group of the entity -}
    | Parent
    | Fixed String
    | Quantity
    | Flow
    | Provider
    | Receiver
    | EventList { qtySep : Separator, eventSep : Separator }


toString : Fragment -> String
toString fragment =
    case fragment of
        IdentifierName identifier ->
            identifier.name

        GroupIdentifierName groupidentifier ->
            groupidentifier.name

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

        EventList _ ->
            "EventList"


toDesc : Fragment -> String
toDesc fragment =
    case fragment of
        IdentifierName _ ->
            "Identifier"

        GroupIdentifierName groupidentifier ->
            "Identifier of a " ++ Scope.toString groupidentifier.scope

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

        EventList _ ->
            "Display the list of partial Events, separated by configurable separators."


encode : Fragment -> Encode.Value
encode fragment =
    case fragment of
        IdentifierName identifier ->
            Encode.object
                [ ( "type", Encode.string "IdentifierName" ), ( "name", Encode.string identifier.name ) ]

        GroupIdentifierName groupidentifier ->
            Encode.object
                [ ( "type", Encode.string "GroupIdentifierName" )
                , ( "scope", Scope.encode groupidentifier.scope )
                , ( "name", Encode.string groupidentifier.name )
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

        EventList separators ->
            Encode.object
                [ ( "type", Encode.string "EventList" )
                , ( "qtySep", Encode.string separators.qtySep )
                , ( "eventSep", Encode.string separators.eventSep )
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
                        Decode.map (\name -> IdentifierName { name = name })
                            (Decode.field "name" Decode.string)

                    "GroupIdentifierName" ->
                        Decode.map2 (\scope name -> GroupIdentifierName { scope = scope, name = name })
                            (Decode.field "scope" Scope.decoder)
                            (Decode.field "name" Decode.string)

                    "EventList" ->
                        Decode.map2 (\qs es -> EventList { qtySep = qs, eventSep = es })
                            (Decode.field "qtySep" Decode.string)
                            (Decode.field "eventSep" Decode.string)

                    _ ->
                        Decode.fail ("Unknown fragment: " ++ t)
            )
