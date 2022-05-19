port module Event exposing (Event(..), EventBase, base, compare, decodelist, decoder, encode, exceptCI, getTime, readEvents, storeEvents, storeEventsToSend)

import DictSet as Set
import EventFlow exposing (EventFlow, decoder)
import IOStatus exposing (IOStatus(..))
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Entity exposing (Entity)
import REA.Event as E
import REA.EventType as ET exposing (EventType)
import REA.Identifier as Identifier exposing (Identifier)
import REA.Identifier.Portion as Portion exposing (Portion)
import REA.Process as P exposing (Process)
import REA.ProcessType as PT exposing (ProcessType)
import Result exposing (Result(..))
import Time exposing (millisToPosix, posixToMillis)
import Websocket exposing (WSStatus(..))



-- read events from IDB


port readEvents : Encode.Value -> Cmd msg



-- store events to IDB then send to WS


port storeEvents : Encode.Value -> Cmd msg



-- only store to IDB


port storeEventsToSend : Encode.Value -> Cmd msg



-- application/user events --


type alias EventBase =
    { uuid : Uuid
    , when : Time.Posix
    , flow : EventFlow
    }


type Event
    = ConnectionInitiated { lastEventTime : Time.Posix, uuids : Set.DictSet String Uuid } EventBase
    | ProcessTypeChanged { name : ProcessType } EventBase
    | ProcessTypeRemoved String EventBase
    | ProcessAdded { type_ : String, name : String } EventBase
    | CommitmentTypeAdded { commitmentType : CommitmentType } EventBase
    | CommitmentTypeRemoved String EventBase
    | CommitmentAdded { process : Process, commitment : Commitment } EventBase
    | EventTypeAdded { eventType : EventType } EventBase
    | EventTypeRemoved String EventBase
    | EventAdded { process : Process, event : E.Event } EventBase
    | LinkedEventTypeToProcessType { etype : String, ptype : String } EventBase
    | LinkedCommitmentTypeToProcessType { ctype : String, ptype : String } EventBase
    | GroupAdded { name : String, entity : Entity } EventBase
    | GroupRemoved { name : String, entity : Entity } EventBase
    | IdentifierAdded { name : String, entity : Entity, unique : Bool, mandatory : Bool, format : List Portion } EventBase
    | IdentifierRemoved String EventBase --TODO also simplify other *Removed
    | AgentTypeAdded { name : String, type_ : Maybe String } EventBase
    | AgentTypeRemoved String EventBase
    | AgentAdded { name : String, type_ : String } EventBase
    | AgentRemoved String EventBase


base : Event -> EventBase
base event =
    case event of
        ProcessTypeChanged _ b ->
            b

        ProcessTypeRemoved _ b ->
            b

        ProcessAdded _ b ->
            b

        CommitmentTypeAdded _ b ->
            b

        CommitmentTypeRemoved _ b ->
            b

        CommitmentAdded _ b ->
            b

        EventTypeAdded _ b ->
            b

        EventTypeRemoved _ b ->
            b

        EventAdded _ b ->
            b

        LinkedEventTypeToProcessType _ b ->
            b

        LinkedCommitmentTypeToProcessType _ b ->
            b

        ConnectionInitiated _ b ->
            b

        GroupAdded _ b ->
            b

        GroupRemoved _ b ->
            b

        IdentifierAdded _ b ->
            b

        IdentifierRemoved _ b ->
            b

        AgentTypeAdded _ b ->
            b

        AgentTypeRemoved _ b ->
            b

        AgentAdded _ b ->
            b

        AgentRemoved _ b ->
            b


compare : Event -> Int
compare =
    getTime >> posixToMillis


getTime : Event -> Time.Posix
getTime =
    base >> .when


exceptCI : List Event -> List Event
exceptCI es =
    List.filter
        (\e ->
            case e of
                ConnectionInitiated _ _ ->
                    False

                _ ->
                    True
        )
        es



-- Event constructors


processTypeChanged : Uuid -> Time.Posix -> EventFlow -> ProcessType -> Event
processTypeChanged uuid when flow name =
    ProcessTypeChanged
        { name = name
        }
        (EventBase uuid when flow)


processTypeRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
processTypeRemoved uuid when flow name =
    ProcessTypeRemoved name
        (EventBase uuid when flow)


processAdded : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
processAdded uuid when flow name type_ =
    ProcessAdded
        { name = name
        , type_ = type_
        }
        (EventBase uuid when flow)


commitmentTypeRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
commitmentTypeRemoved uuid when flow name =
    CommitmentTypeRemoved name (EventBase uuid when flow)


commitmentTypeAdded : Uuid -> Time.Posix -> EventFlow -> CommitmentType -> Event
commitmentTypeAdded uuid when flow commitmentType =
    CommitmentTypeAdded
        { commitmentType = commitmentType
        }
        (EventBase uuid when flow)


commitmentAdded : Uuid -> Time.Posix -> EventFlow -> Process -> Commitment -> Event
commitmentAdded uuid when flow process commitment =
    CommitmentAdded
        { commitment = commitment
        , process = process
        }
        (EventBase uuid when flow)


eventTypeRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
eventTypeRemoved uuid when flow name =
    EventTypeRemoved name (EventBase uuid when flow)


linkedEventTypeToProcessType : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
linkedEventTypeToProcessType uuid when flow etype ptype =
    LinkedEventTypeToProcessType
        { etype = etype
        , ptype = ptype
        }
        (EventBase uuid when flow)


linkedCommitmentTypeToProcessType : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
linkedCommitmentTypeToProcessType uuid when flow ctype ptype =
    LinkedCommitmentTypeToProcessType
        { ctype = ctype
        , ptype = ptype
        }
        (EventBase uuid when flow)


eventTypeAdded : Uuid -> Time.Posix -> EventFlow -> EventType -> Event
eventTypeAdded uuid when flow eventType =
    EventTypeAdded
        { eventType = eventType
        }
        (EventBase uuid when flow)


eventAdded : Uuid -> Time.Posix -> EventFlow -> Process -> E.Event -> Event
eventAdded uuid when flow process event =
    EventAdded
        { event = event
        , process = process
        }
        (EventBase uuid when flow)


connectionInitiated : Uuid -> Time.Posix -> EventFlow -> Time.Posix -> Set.DictSet String Uuid -> Event
connectionInitiated uuid when flow lastEventTime uuids =
    ConnectionInitiated
        { lastEventTime = lastEventTime
        , uuids = uuids
        }
        (EventBase uuid when flow)


groupAdded : Uuid -> Time.Posix -> EventFlow -> String -> Entity -> Event
groupAdded uuid when flow name entity =
    GroupAdded
        { name = name
        , entity = entity
        }
        (EventBase uuid when flow)


groupRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Entity -> Event
groupRemoved uuid when flow name entity =
    GroupRemoved
        { name = name
        , entity = entity
        }
        (EventBase uuid when flow)


identifierAdded : Uuid -> Time.Posix -> EventFlow -> String -> Entity -> Bool -> Bool -> List Portion -> Event
identifierAdded uuid when flow name entity unique mandatory format =
    IdentifierAdded
        { name = name
        , entity = entity
        , unique = unique
        , mandatory = mandatory
        , format = format
        }
        (EventBase uuid when flow)


identifierRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
identifierRemoved uuid when flow name =
    IdentifierRemoved name (EventBase uuid when flow)


agentTypeAdded : Uuid -> Time.Posix -> EventFlow -> String -> Maybe String -> Event
agentTypeAdded uuid when flow name type_ =
    AgentTypeAdded { name = name, type_ = type_ } (EventBase uuid when flow)


agentTypeRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
agentTypeRemoved uuid when flow name =
    AgentTypeRemoved name (EventBase uuid when flow)


agentAdded : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
agentAdded uuid when flow name type_ =
    AgentAdded { name = name, type_ = type_ } (EventBase uuid when flow)


agentRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
agentRemoved uuid when flow name =
    AgentRemoved name (EventBase uuid when flow)



-- JSON encoding / decoding


encode : Event -> Encode.Value
encode event =
    case event of
        ProcessTypeChanged e b ->
            Encode.object
                [ ( "what", Encode.string "ProcessTypeChanged" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", PT.encode e.name )
                ]

        ProcessTypeRemoved e b ->
            Encode.object
                [ ( "what", Encode.string "ProcessTypeRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e )
                ]

        ProcessAdded e b ->
            Encode.object
                [ ( "what", Encode.string "ProcessAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "type", Encode.string e.type_ )
                ]

        CommitmentTypeAdded e b ->
            Encode.object
                [ ( "what", Encode.string "CommitmentTypeAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentTypeRemoved e b ->
            Encode.object
                [ ( "what", Encode.string "CommitmentTypeRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e )
                ]

        CommitmentAdded e b ->
            Encode.object
                [ ( "what", Encode.string "CommitmentAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "process", P.encode e.process )
                , ( "commitment", C.encode e.commitment )
                ]

        EventTypeAdded e b ->
            Encode.object
                [ ( "what", Encode.string "EventTypeAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "eventType", ET.encode e.eventType )
                ]

        LinkedEventTypeToProcessType e b ->
            Encode.object
                [ ( "what", Encode.string "LinkedEventTypeToProcessType" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "etype", Encode.string e.etype )
                , ( "ptype", Encode.string e.ptype )
                ]

        LinkedCommitmentTypeToProcessType e b ->
            Encode.object
                [ ( "what", Encode.string "LinkedCommitmentTypeToProcessType" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "ctype", Encode.string e.ctype )
                , ( "ptype", Encode.string e.ptype )
                ]

        EventTypeRemoved e b ->
            Encode.object
                [ ( "what", Encode.string "EventTypeRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e )
                ]

        EventAdded e b ->
            Encode.object
                [ ( "what", Encode.string "EventAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "process", P.encode e.process )
                , ( "event", E.encode e.event )
                ]

        ConnectionInitiated e b ->
            Encode.object
                [ ( "what", Encode.string "ConnectionInitiated" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "lastEventTime", Encode.int <| posixToMillis e.lastEventTime )
                , ( "uuids", Encode.list Uuid.encode <| Set.toList e.uuids )
                ]

        GroupAdded e b ->
            Encode.object
                [ ( "what", Encode.string "GroupAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "entity", REA.Entity.encode e.entity )
                ]

        GroupRemoved e b ->
            Encode.object
                [ ( "what", Encode.string "GroupRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "entity", REA.Entity.encode e.entity )
                ]

        IdentifierAdded e b ->
            Encode.object
                [ ( "what", Encode.string "IdentifierAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "entity", REA.Entity.encode e.entity )
                , ( "unique", Encode.bool e.unique )
                , ( "mandatory", Encode.bool e.mandatory )
                , ( "format", Encode.list Portion.encode e.format )
                ]

        IdentifierRemoved e b ->
            Encode.object
                [ ( "what", Encode.string "IdentifierRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e )
                ]

        AgentTypeAdded e b ->
            Encode.object
                [ ( "what", Encode.string "AgentTypeAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "type", Maybe.map Encode.string e.type_ |> Maybe.withDefault Encode.null )
                ]

        AgentTypeRemoved e b ->
            Encode.object
                [ ( "what", Encode.string "AgentTypeRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e )
                ]

        AgentAdded e b ->
            Encode.object
                [ ( "what", Encode.string "AgentAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "type", Encode.string e.type_ )
                ]

        AgentRemoved e b ->
            Encode.object
                [ ( "what", Encode.string "AgentRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "when", Encode.int <| posixToMillis b.when )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e )
                ]


decodelist : Decode.Value -> List Event
decodelist =
    Result.withDefault [] << decodeValue (Decode.list decoder)


decoder : Decoder Event
decoder =
    let
        toPosix : Int -> Decoder Time.Posix
        toPosix t =
            Decode.succeed (millisToPosix t)
    in
    Decode.field "what" Decode.string
        |> andThen
            (\s ->
                case s of
                    "ProcessTypeChanged" ->
                        Decode.map4 processTypeChanged
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "type" PT.decoder)

                    "ProcessTypeRemoved" ->
                        Decode.map4 processTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "type" Decode.string)

                    "ProcessAdded" ->
                        Decode.map5 processAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "type" Decode.string)

                    "CommitmentTypeAdded" ->
                        Decode.map4 commitmentTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "commitmentType" CT.decoder)

                    "CommitmentTypeRemoved" ->
                        Decode.map4 commitmentTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)

                    "CommitmentAdded" ->
                        Decode.map5 commitmentAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "process" P.decoder)
                            (Decode.field "commitment" C.decoder)

                    "EventTypeAdded" ->
                        Decode.map4 eventTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "eventType" ET.decoder)

                    "LinkedEventTypeToProcessType" ->
                        Decode.map5 linkedEventTypeToProcessType
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "etype" Decode.string)
                            (Decode.field "ptype" Decode.string)

                    "LinkedCommitmentTypeToProcessType" ->
                        Decode.map5 linkedCommitmentTypeToProcessType
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "ctype" Decode.string)
                            (Decode.field "ptype" Decode.string)

                    "EventTypeRemoved" ->
                        Decode.map4 eventTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)

                    "EventAdded" ->
                        Decode.map5 eventAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "process" P.decoder)
                            (Decode.field "event" E.decoder)

                    "ConnectionInitiated" ->
                        Decode.map5 connectionInitiated
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "lastEventTime" Decode.int |> andThen toPosix)
                            (Decode.field "uuids" (Decode.list Uuid.decoder) |> andThen (\xs -> Decode.succeed (Set.fromList Uuid.toString xs)))

                    "GroupAdded" ->
                        Decode.map5 groupAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "entity" REA.Entity.decoder)

                    "GroupRemoved" ->
                        Decode.map5 groupRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "entity" REA.Entity.decoder)

                    "IdentifierAdded" ->
                        Decode.map8 identifierAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "entity" REA.Entity.decoder)
                            (Decode.field "unique" Decode.bool)
                            (Decode.field "mandatory" Decode.bool)
                            (Decode.field "format" (Decode.list Portion.decoder))

                    "IdentifierRemoved" ->
                        Decode.map4 identifierRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)

                    "AgentTypeAdded" ->
                        Decode.map5 agentTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "type" (Decode.maybe Decode.string))

                    "AgentTypeRemoved" ->
                        Decode.map4 agentTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)

                    "AgentAdded" ->
                        Decode.map5 agentAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "type" Decode.string)

                    "AgentRemoved" ->
                        Decode.map4 agentRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "when" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail "Unknown Event type"
            )
