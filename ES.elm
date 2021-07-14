port module ES exposing (Event, EventType(..), State, aggregate, decode, encode, getEvents, getProcess, intToPosix, storeEvent)

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder, andThen)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Entity as Ent exposing (Entity)
import REA.Process exposing (Process)
import REA.ProcessType exposing (ProcessType)
import Random.Pcg.Extended exposing (Seed)
import Result exposing (Result(..))
import Route exposing (Route)
import Status exposing (Status(..))
import Time


port getEvents : Encode.Value -> Cmd msg


port storeEvent : Encode.Value -> Cmd msg



-- global application state --


type alias State =
    { currentSeed : Seed
    , currentUuid : Uuid
    , navkey : Nav.Key
    , route : Route
    , processes : List Process
    , processType : ProcessType
    }



-- business events --


type EventType
    = CommitmentAdded
    | ProcessAdded
    | EventAdded


type alias Event =
    { uuid : Uuid.Uuid
    , posixtime : Time.Posix
    , etype : EventType
    , entity : Entity
    , entityType : String
    }


getProcess : State -> Maybe Process
getProcess state =
    case state.route of
        Route.Process str ->
            case Uuid.fromString str of
                Just uuid ->
                    List.foldr
                        (\p m ->
                            if uuid == p.uuid then
                                Just p

                            else
                                m
                        )
                        Nothing
                        state.processes

                Nothing ->
                    Nothing

        _ ->
            Nothing


getUuid : State -> Maybe Uuid
getUuid state =
    case state.route of
        Route.Process str ->
            Uuid.fromString str

        _ ->
            Nothing



--- evolve the state given an event


aggregate : Event -> State -> State
aggregate event state =
    case event.etype of
        ProcessAdded ->
            case Ent.toProcess event.entity of
                Nothing ->
                    state

                Just p ->
                    { state | processes = p :: state.processes }

        CommitmentAdded ->
            case Ent.toCommitment event.entity of
                Just c ->
                    let
                        uuid =
                            getUuid state

                        processes =
                            List.map
                                (\p ->
                                    if Just p.uuid == uuid then
                                        { p | commitments = c :: p.commitments }

                                    else
                                        p
                                )
                                state.processes
                    in
                    { state | processes = processes }

                Nothing ->
                    state

        _ ->
            state


eventTypeEncode : EventType -> Encode.Value
eventTypeEncode etype =
    case etype of
        ProcessAdded ->
            Encode.string "ProcessAdded"

        CommitmentAdded ->
            Encode.string "CommitmentAdded"

        EventAdded ->
            Encode.string "EventAdded"


eventTypeDecoder : Decoder EventType
eventTypeDecoder =
    Decode.string
        |> andThen
            (\s ->
                case s of
                    "ProcessAdded" ->
                        Decode.succeed ProcessAdded

                    "CommitmentAdded" ->
                        Decode.succeed CommitmentAdded

                    "EventAdded" ->
                        Decode.succeed EventAdded

                    _ ->
                        Decode.fail "Unknown Event type"
            )


encode : Event -> Encode.Value
encode event =
    Encode.object
        [ ( "uuid", Uuid.encode event.uuid )
        , ( "posixtime", Encode.int <| Time.posixToMillis event.posixtime )
        , ( "etype", eventTypeEncode event.etype )
        , ( "entityType", Encode.string event.entityType )
        , ( "entity", Ent.encode event.entity )
        ]


decode : Decoder Event
decode =
    Decode.map5 Event
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "posixtime" Decode.int |> andThen intToPosix)
        (Decode.field "etype" eventTypeDecoder)
        (Decode.field "entityType" Decode.string
            |> andThen Ent.decode
        )
        (Decode.field "entityType" Decode.string)


intToPosix : Int -> Decode.Decoder Time.Posix
intToPosix millis =
    Decode.succeed <| Time.millisToPosix millis
