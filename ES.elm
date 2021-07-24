port module ES exposing (Event(..), State, aggregate, compare, decoder, encode, getCommitments, getEvents, getProcess, new, storeEvent)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder, andThen)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Event as E
import REA.Process as P exposing (Process)
import REA.ProcessCommitments as PC exposing (ProcessCommitments)
import REA.ProcessEvents as PE exposing (ProcessEvents)
import Random.Pcg.Extended exposing (Seed)
import Result exposing (Result(..))
import Route exposing (Route)
import Status exposing (Status(..))
import Time exposing (millisToPosix, posixToMillis)


port getEvents : Encode.Value -> Cmd msg


port storeEvent : Encode.Value -> Cmd msg



-- application/user events --


type Event
    = ProcessAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , process : Process
        }
    | CommitmentTypeAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , commitmentType : CommitmentType
        }
    | CommitmentTypeRemoved
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , commitmentType : CommitmentType
        }
    | CommitmentAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , process : Process
        , commitment : Commitment
        }
    | EventAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , process : Process
        , event : E.Event
        }



-- global application state --


type alias State =
    { currentSeed : Seed
    , navkey : Nav.Key
    , route : Route
    , status : Status
    , processes : DictSet Int Process
    , commitments : DictSet Int Commitment
    , commitmentTypes : DictSet String CommitmentType
    , events : DictSet String E.Event
    , process_commitments : DictSet Int ProcessCommitments
    , process_events : DictSet String ProcessEvents
    , inputCommitmentType : String
    }


new : Seed -> Nav.Key -> Route -> State
new seed key route =
    { currentSeed = seed
    , navkey = key
    , route = route
    , status = Loading
    , commitments = Set.empty C.compare
    , processes = Set.empty P.compare
    , process_commitments = Set.empty PC.compare
    , commitmentTypes = Set.empty CT.compare
    , process_events = Set.empty PE.compare
    , events = Set.empty E.compare
    , inputCommitmentType = ""
    }


compare : Event -> Int
compare event =
    case event of
        ProcessAdded p ->
            posixToMillis p.posixtime

        CommitmentTypeAdded ct ->
            posixToMillis ct.posixtime

        CommitmentTypeRemoved ct ->
            posixToMillis ct.posixtime

        CommitmentAdded c ->
            posixToMillis c.posixtime

        EventAdded e ->
            posixToMillis e.posixtime


getProcess : State -> String -> Maybe Process
getProcess state str =
    Uuid.fromString str
        |> Maybe.andThen
            (\uuid ->
                Set.filter
                    (\p -> p.uuid == uuid)
                    state.processes
                    |> Set.values
                    |> List.head
            )


getCommitments : State -> Process -> DictSet Int Commitment
getCommitments state process =
    Set.filter (\pc -> pc.process.uuid == process.uuid) state.process_commitments
        |> Set.map C.compare (\pc -> pc.commitment)



--- evolve the state given an event


aggregate : Event -> State -> State
aggregate event state =
    case event of
        ProcessAdded e ->
            { state | processes = Set.insert e.process state.processes }

        CommitmentTypeAdded e ->
            { state
                | commitmentTypes = Set.insert e.commitmentType state.commitmentTypes
            }

        CommitmentTypeRemoved e ->
            { state
                | commitmentTypes = Set.remove e.commitmentType state.commitmentTypes
            }

        CommitmentAdded e ->
            { state
                | commitments = Set.insert e.commitment state.commitments
                , process_commitments = Set.insert { process = e.process, commitment = e.commitment } state.process_commitments
            }

        EventAdded e ->
            { state
                | events = Set.insert e.event state.events
                , process_events = Set.insert { process = e.process, event = e.event } state.process_events
            }


encode : Event -> Encode.Value
encode event =
    case event of
        ProcessAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "ProcessAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "process", P.encode e.process )
                ]

        CommitmentTypeAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "CommitmentTypeAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "CommitmentTypeRemoved" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "CommitmentAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "process", P.encode e.process )
                , ( "commitment", C.encode e.commitment )
                ]

        EventAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "EventAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "process", P.encode e.process )
                , ( "event", E.encode e.event )
                ]


processAdded : Uuid -> Time.Posix -> Process -> Event
processAdded uuid posixtime process =
    ProcessAdded
        { uuid = uuid
        , posixtime = posixtime
        , process = process
        }



-- Event constructors


commitmentTypeRemoved : Uuid -> Time.Posix -> CommitmentType -> Event
commitmentTypeRemoved uuid posixtime commitmentType =
    CommitmentTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , commitmentType = commitmentType
        }


commitmentTypeAdded : Uuid -> Time.Posix -> CommitmentType -> Event
commitmentTypeAdded uuid posixtime commitmentType =
    CommitmentTypeAdded
        { uuid = uuid
        , posixtime = posixtime
        , commitmentType = commitmentType
        }


commitmentAdded : Uuid -> Time.Posix -> Process -> Commitment -> Event
commitmentAdded uuid posixtime process commitment =
    CommitmentAdded
        { uuid = uuid
        , posixtime = posixtime
        , commitment = commitment
        , process = process
        }


eventAdded : Uuid -> Time.Posix -> Process -> E.Event -> Event
eventAdded uuid posixtime process event =
    EventAdded
        { uuid = uuid
        , posixtime = posixtime
        , event = event
        , process = process
        }


decoder : Decoder Event
decoder =
    let
        toPosix t =
            millisToPosix t |> Decode.succeed
    in
    Decode.field "type" Decode.string
        |> andThen
            (\s ->
                case s of
                    "ProcessAdded" ->
                        Decode.map3 processAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "process" P.decoder)

                    "CommitmentTypeAdded" ->
                        Decode.map3 commitmentTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "commitmentType" CT.decoder)

                    "CommitmentTypeRemoved" ->
                        Decode.map3 commitmentTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "commitmentType" CT.decoder)

                    "CommitmentAdded" ->
                        Decode.map4 commitmentAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "process" P.decoder)
                            (Decode.field "commitment" C.decoder)

                    "EventAdded" ->
                        Decode.map4 eventAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "process" P.decoder)
                            (Decode.field "event" E.decoder)

                    _ ->
                        Decode.fail "Unknown Event type"
            )
