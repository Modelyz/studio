port module ES exposing (Event, State, aggregate, decode, encode, getEvents, getProcess, intToPosix, storeEvent)

import Browser.Navigation as Nav
import Json.Decode exposing (andThen)
import Json.Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Entity as Ent exposing (Entity)
import REA.Process exposing (Process)
import REA.ProcessType exposing (ProcessType)
import Random.Pcg.Extended exposing (Seed)
import Route exposing (Route)
import Status exposing (Status(..))
import Time


port getEvents : Json.Encode.Value -> Cmd msg


port storeEvent : Json.Encode.Value -> Cmd msg



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


type alias Event =
    { uuid : Uuid.Uuid
    , posixtime : Time.Posix
    , name : String
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
    case event.name of
        "Process added" ->
            -- TODO turn this into a type
            case Ent.toProcess event.entity of
                Nothing ->
                    state

                Just p ->
                    { state | processes = p :: state.processes }

        "Commitment added" ->
            -- TODO turn this into a type
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


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ( "uuid", Uuid.encode event.uuid )
        , ( "posixtime", Json.Encode.int <| Time.posixToMillis event.posixtime )
        , ( "name", Json.Encode.string event.name )
        , ( "entityType", Json.Encode.string event.entityType )
        , ( "entity", Ent.encode event.entity )
        ]


decode : Json.Decode.Decoder Event
decode =
    Json.Decode.map5 Event
        (Json.Decode.field "uuid" Uuid.decoder)
        (Json.Decode.field "posixtime" Json.Decode.int |> andThen intToPosix)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "entityType" Json.Decode.string
            |> andThen Ent.decode
        )
        (Json.Decode.field "entityType" Json.Decode.string)


intToPosix : Int -> Json.Decode.Decoder Time.Posix
intToPosix millis =
    Json.Decode.succeed <| Time.millisToPosix millis
