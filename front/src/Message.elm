port module Message exposing (Message(..), Metadata, Payload(..), base, compare, decodelist, decoder, encode, exceptCI, getTime, readMessages, storeMessages, storeMessagesToSend)

import Configuration exposing (Configuration)
import DictSet as Set
import Entity.Entity as Entity exposing (Entity)
import Group.Group as Group exposing (Group)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import MessageFlow exposing (MessageFlow, decoder)
import Prng.Uuid as Uuid exposing (Uuid)
import Restriction.Restriction as Restriction exposing (Restriction)
import Time exposing (millisToPosix, posixToMillis)



-- read messages from IDB


port readMessages : Encode.Value -> Cmd msg



-- store messages to IDB then send to WS


port storeMessages : Encode.Value -> Cmd msg



-- only store to IDB


port storeMessagesToSend : Encode.Value -> Cmd msg



-- application/user messages --


type alias Metadata =
    { uuid : Uuid
    , when : Time.Posix
    , flow : MessageFlow
    }


type Message
    = Message Metadata Payload


type Payload
    = ConnectionInitiated Connection
    | Added Entity
    | Removed Entity
    | Restricted Restriction
    | IdentifierTypeAdded IdentifierType
    | IdentifierTypeRemoved IdentifierType
    | IdentifierAdded Identifier
    | Configured Configuration
    | Unconfigured Configuration
    | DefinedGroup Group -- TODO remove Group as Entity?
    | RemovedGroup Group
    | Grouped Entity Group
    | Ungrouped Entity Group


toString : Payload -> String
toString p =
    case p of
        ConnectionInitiated _ ->
            "ConnectionInitiated"

        Restricted _ ->
            "Restricted"

        IdentifierTypeAdded _ ->
            "IdentifierTypeAdded"

        IdentifierTypeRemoved _ ->
            "IdentifierTypeRemoved"

        Added _ ->
            "Added"

        Removed _ ->
            "Removed"

        IdentifierAdded _ ->
            "IdentifierAdded"

        Configured _ ->
            "Configured"

        Unconfigured _ ->
            "Unconfigured"

        DefinedGroup _ ->
            "DefinedGroup"

        RemovedGroup _ ->
            "RemovedGroup"

        Grouped _ _ ->
            "Grouped"

        Ungrouped _ _ ->
            "Ungrouped"


type alias Connection =
    -- TODO move in its module?
    { lastMessageTime : Time.Posix, uuids : Set.DictSet String Uuid }


base : Message -> Metadata
base (Message b p) =
    b


compare : Message -> Int
compare =
    getTime >> posixToMillis


getTime : Message -> Time.Posix
getTime =
    base >> .when


exceptCI : List Message -> List Message
exceptCI es =
    List.filter
        (\(Message b p) ->
            case p of
                ConnectionInitiated _ ->
                    False

                _ ->
                    True
        )
        es



-- JSON encoding / decoding


encodeBase : Metadata -> Encode.Value
encodeBase b =
    Encode.object
        [ ( "uuid", Uuid.encode b.uuid )
        , ( "when", Encode.int <| posixToMillis b.when )
        , ( "flow", MessageFlow.encode b.flow )
        ]


encode : Message -> Encode.Value
encode (Message b p) =
    Encode.object
        [ ( "what", Encode.string <| toString p )
        , ( "meta", encodeBase b )
        , case p of
            Restricted r ->
                ( "load", Restriction.encode r )

            ConnectionInitiated e ->
                ( "load"
                , Encode.object
                    [ ( "lastMessageTime", Encode.int <| posixToMillis e.lastMessageTime )
                    , ( "uuids", Encode.list Uuid.encode <| Set.toList e.uuids )
                    ]
                )

            IdentifierTypeAdded it ->
                ( "load", IdentifierType.encode it )

            IdentifierTypeRemoved it ->
                ( "load", IdentifierType.encode it )

            Added e ->
                ( "load", Entity.encode e )

            Removed e ->
                ( "load", Entity.encode e )

            IdentifierAdded i ->
                ( "load", Identifier.encode i )

            Configured c ->
                ( "load", Configuration.encode c )

            Unconfigured c ->
                ( "load", Configuration.encode c )

            DefinedGroup g ->
                ( "load", Group.encode g )

            RemovedGroup g ->
                ( "load", Group.encode g )

            Grouped e g ->
                ( "load", Encode.object [ ( "entity", Entity.encode e ), ( "group", Group.encode g ) ] )

            Ungrouped e g ->
                ( "load", Encode.object [ ( "entity", Entity.encode e ), ( "group", Group.encode g ) ] )
        ]


decodelist : Decode.Value -> List Message
decodelist =
    Result.withDefault [] << decodeValue (Decode.list decoder)


toPosix : Int -> Decoder Time.Posix
toPosix t =
    Decode.succeed (millisToPosix t)


baseDecoder : Decoder Metadata
baseDecoder =
    Decode.map3 Metadata
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> andThen toPosix)
        (Decode.field "flow" MessageFlow.decoder)


decoder : Decoder Message
decoder =
    Decode.map2 Message
        (Decode.field "meta" baseDecoder)
        (Decode.field "what" Decode.string
            |> andThen
                (\t ->
                    case t of
                        "Restricted" ->
                            Decode.map Restricted
                                (Decode.field "load" Restriction.decoder)

                        "ConnectionInitiated" ->
                            Decode.map ConnectionInitiated
                                (Decode.field "load"
                                    (Decode.map2 Connection
                                        (Decode.field "lastMessageTime" Decode.int |> andThen toPosix)
                                        (Decode.field "uuids" (Decode.list Uuid.decoder) |> andThen (\xs -> Decode.succeed (Set.fromList Uuid.toString xs)))
                                    )
                                )

                        "IdentifierTypeAdded" ->
                            Decode.map IdentifierTypeAdded
                                (Decode.field "load" IdentifierType.decoder)

                        "IdentifierTypeRemoved" ->
                            Decode.map IdentifierTypeRemoved
                                (Decode.field "load" IdentifierType.decoder)

                        "Added" ->
                            Decode.map Added
                                (Decode.field "load" Entity.decoder)

                        "Removed" ->
                            Decode.map Removed
                                (Decode.field "load" Entity.decoder)

                        "IdentifierAdded" ->
                            Decode.map IdentifierAdded
                                (Decode.field "load" Identifier.decoder)

                        "Configured" ->
                            Decode.map Configured
                                (Decode.field "load" Configuration.decoder)

                        "Unconfigured" ->
                            Decode.map Unconfigured
                                (Decode.field "load" Configuration.decoder)

                        "DefinedGroup" ->
                            Decode.map DefinedGroup (Decode.field "group" Group.decoder)

                        "RemovedGroup" ->
                            Decode.map RemovedGroup (Decode.field "group" Group.decoder)

                        "Grouped" ->
                            Decode.map2 Grouped (Decode.field "entity" Entity.decoder) (Decode.field "group" Group.decoder)

                        "Ungrouped" ->
                            Decode.map2 Ungrouped (Decode.field "entity" Entity.decoder) (Decode.field "group" Group.decoder)

                        _ ->
                            Decode.fail <| "Unknown Message type: " ++ t
                )
        )
