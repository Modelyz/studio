port module Message exposing (Message(..), Metadata, Payload(..), base, compare, decodelist, decoder, encode, exceptCI, getTime, readMessages, storeMessages, storeMessagesToSend)

import Commitment.Commitment as CM exposing (Commitment)
import DictSet as Set
import Entity.Entity as EN exposing (Entity)
import EntityType.EntityType as ENT exposing (EntityType)
import Group.Group as Group exposing (Group)
import GroupType.GroupType as GroupType exposing (GroupType)
import Ident.EntityIdentifier as EntityIdentifier exposing (EntityIdentifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import MessageFlow exposing (MessageFlow, decoder)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as P exposing (Process)
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
    | ProcessTypeChanged EntityType
    | ProcessTypeRemoved String
    | Restricted Restriction
    | IdentifierTypeAdded IdentifierType
    | IdentifierTypeRemoved IdentifierType
    | Added Entity
    | Removed Entity
    | TypeAdded EntityType
    | TypeRemoved EntityType
    | IdentifierAdded EntityIdentifier


toString : Payload -> String
toString p =
    case p of
        ConnectionInitiated _ ->
            "ConnectionInitiated"

        ProcessTypeChanged _ ->
            "ProcessTypeChanged"

        ProcessTypeRemoved _ ->
            "ProcessTypeRemoved"

        TypeRemoved _ ->
            "TypeRemoved"

        Restricted _ ->
            "Restricted"

        IdentifierTypeAdded _ ->
            "IdentifierTypeAdded"

        IdentifierTypeRemoved _ ->
            "IdentifierTypeRemoved"

        TypeAdded _ ->
            "TypeAdded"

        Added _ ->
            "Added"

        Removed _ ->
            "Removed"

        IdentifierAdded _ ->
            "IdentifierAdded"


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
            ProcessTypeChanged pt ->
                ( "load", ENT.encode pt )

            ProcessTypeRemoved pt ->
                ( "load", Encode.string pt )

            Restricted r ->
                ( "load", Restriction.encode r )

            TypeRemoved et ->
                ( "load", ENT.encode et )

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

            TypeAdded e ->
                ( "load", ENT.encode e )

            Added e ->
                ( "load", EN.encode e )

            Removed e ->
                ( "load", EN.encode e )

            IdentifierAdded ei ->
                ( "load", EntityIdentifier.encode ei )
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
                        "ProcessTypeChanged" ->
                            Decode.map ProcessTypeChanged
                                (Decode.field "load" ENT.decoder)

                        "ProcessTypeRemoved" ->
                            Decode.map ProcessTypeRemoved
                                (Decode.field "load" Decode.string)

                        "Restricted" ->
                            Decode.map Restricted
                                (Decode.field "load" Restriction.decoder)

                        "TypeRemoved" ->
                            Decode.map TypeRemoved
                                (Decode.field "load" ENT.decoder)

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

                        "TypeAdded" ->
                            Decode.map TypeAdded
                                (Decode.field "load" ENT.decoder)

                        "Added" ->
                            Decode.map Added
                                (Decode.field "load" EN.decoder)

                        "Removed" ->
                            Decode.map Removed
                                (Decode.field "load" EN.decoder)

                        "IdentifierAdded" ->
                            Decode.map IdentifierAdded
                                (Decode.field "load" EntityIdentifier.decoder)

                        _ ->
                            Decode.fail <| "Unknown Message type: " ++ t
                )
        )
