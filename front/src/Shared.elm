module Shared exposing (Model, Msg(..), dispatch, dispatchMany, dispatchT, identity, init, update)

import Browser.Navigation as Nav
import DictSet as Set
import Effect exposing (Effect)
import Event exposing (Event(..), EventBase, exceptCI, getTime)
import EventFlow as Flow
import IOStatus exposing (IOStatus(..))
import Json.Decode as Json exposing (decodeString, decodeValue, errorToString)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Process
import Random.Pcg.Extended as Random exposing (initialSeed)
import Route exposing (Route)
import State exposing (State)
import Task
import Time exposing (millisToPosix, posixToMillis)
import Websocket as WS exposing (WSStatus(..), wsConnect, wsSend)


type alias Model =
    State


type Msg
    = None ()
    | WSDisconnected Json.Value
    | WSError Json.Value
    | WSConnect ()
    | WSConnected Json.Value
    | ReplaceRoute Route
    | PushRoute Route
    | StoreEventsToSend (List Event.Event)
    | SendEvents (List Event.Event)
    | EventsStored Json.Value
    | EventsStoredTosend Json.Value
    | EventsRead Json.Value
    | EventsSent Json.Value
    | EventsReceived String


init : ( Int, List Int ) -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) navkey =
    ( State.new (initialSeed seed seedExtension) navkey
    , Event.readEvents Encode.null
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None _ ->
            ( model, Cmd.none )

        WSConnect _ ->
            ( model, wsConnect () )

        WSConnected st ->
            let
                wsstatus =
                    WS.fromReadyState st

                timeoutReconnect =
                    if wsstatus == WSOpen then
                        max 1 <| remainderBy 4 (posixToMillis model.lastEventTime)

                    else
                        model.timeoutReconnect

                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed

                cmd =
                    if wsstatus == WSOpen then
                        initiateConnection newUuid model

                    else
                        Cmd.none
            in
            ( { model | currentSeed = newSeed, wsstatus = wsstatus, timeoutReconnect = timeoutReconnect }, cmd )

        WSError _ ->
            ( { model
                | iostatus =
                    IOError "Websocket error"
              }
            , Cmd.none
            )

        WSDisconnected _ ->
            ( { model
                | timeoutReconnect = min 30 (model.timeoutReconnect + 1)
                , wsstatus = WSClosed
              }
            , Task.perform WSConnect
                (Process.sleep (toFloat (1000 * model.timeoutReconnect)))
            )

        EventsRead results ->
            case decodeValue (Json.list Event.decoder) results of
                Ok events ->
                    let
                        newmodel =
                            List.foldr State.aggregate model (List.reverse events)
                    in
                    ( { newmodel
                        | iostatus = IOIdle
                        , wsstatus =
                            case model.wsstatus of
                                WSClosed ->
                                    WSConnecting

                                _ ->
                                    model.wsstatus
                        , lastEventTime =
                            events
                                |> List.map (getTime >> posixToMillis)
                                |> List.maximum
                                |> Maybe.withDefault 0
                                |> millisToPosix
                      }
                    , case model.wsstatus of
                        WSClosed ->
                            wsConnect ()

                        _ ->
                            Cmd.none
                    )

                Err str ->
                    ( { model | iostatus = IOError <| errorToString str }, Cmd.none )

        PushRoute route ->
            ( model, Nav.pushUrl model.navkey <| Route.toUrl route )

        ReplaceRoute route ->
            ( model, Nav.replaceUrl model.navkey <| Route.toUrl route )

        SendEvents events ->
            -- send the new events and the pending ones
            ( { model | iostatus = WSSending }
            , WS.wsSend <|
                Encode.encode 0 <|
                    Encode.list Event.encode <|
                        Set.toList <|
                            Set.union model.pendingEvents <|
                                Set.fromList Event.compare events
            )

        StoreEventsToSend events ->
            let
                ( _, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model | currentSeed = newSeed, iostatus = ESStoring }, Event.storeEventsToSend (Encode.list Event.encode events) )

        EventsStoredTosend events ->
            case decodeValue (Json.list Event.decoder) events of
                Ok evs ->
                    if model.wsstatus == WSOpen then
                        ( { model | iostatus = ESReading }
                        , Cmd.batch
                            [ Event.readEvents Encode.null

                            -- send the new events and the pending ones
                            , wsSend <|
                                Encode.encode 0 <|
                                    Encode.list Event.encode <|
                                        Set.toList <|
                                            Set.union model.pendingEvents <|
                                                Set.fromList Event.compare evs
                            ]
                        )

                    else
                        ( { model | iostatus = IOIdle }, Event.readEvents Encode.null )

                Err err ->
                    ( { model | iostatus = IOError <| errorToString err }, Cmd.none )

        EventsStored _ ->
            ( { model
                | iostatus = IOIdle
              }
            , Event.readEvents Encode.null
            )

        EventsSent status ->
            case decodeValue Json.string status of
                Ok str ->
                    if str == "OK" then
                        ( { model | iostatus = ESReading }, Cmd.none )

                    else
                        ( { model | iostatus = IOError str }, Cmd.none )

                Err err ->
                    ( { model | iostatus = IOError <| errorToString err }, Cmd.none )

        EventsReceived ms ->
            case decodeString (Json.list Event.decoder) ms of
                Ok messages ->
                    let
                        msgs =
                            exceptCI messages
                    in
                    ( { model
                        | wsstatus = WSOpen
                        , iostatus =
                            if List.length msgs > 0 then
                                ESStoring

                            else
                                IOIdle
                      }
                    , if List.length msgs > 0 then
                        Event.storeEvents <|
                            Encode.list Event.encode <|
                                exceptCI messages

                      else
                        Cmd.none
                    )

                Err err ->
                    ( { model | iostatus = IOError <| errorToString err }, Cmd.none )


initiateConnection : Uuid -> Model -> Cmd Msg
initiateConnection uuid model =
    Task.perform SendEvents <|
        Task.map
            (\t ->
                List.singleton <|
                    ConnectionInitiated
                        { lastEventTime = model.lastEventTime
                        , sessionUuid = uuid
                        , uuids = Set.insert uuid model.uuids
                        }
                        { uuid = uuid, posixtime = t, flow = Flow.Requested }
            )
            Time.now


dispatch : Model -> (EventBase -> Event) -> Effect Msg msg
dispatch model payload =
    -- take an Event payload and add the EventBase informations
    let
        ( newUuid, _ ) =
            Random.step Uuid.generator model.currentSeed
    in
    Effect.fromSharedCmd <|
        Task.perform
            StoreEventsToSend
        <|
            Task.map
                (\t -> List.singleton <| payload { uuid = newUuid, posixtime = t, flow = Flow.Requested })
                Time.now


dispatchT : Model -> (Uuid -> Time.Posix -> EventBase -> Event) -> Effect Msg msg
dispatchT model payload =
    -- take an Event payload, feed with a uuid and posixtime, and add the EventBase informations
    let
        ( newUuid, _ ) =
            Random.step Uuid.generator model.currentSeed
    in
    Effect.fromSharedCmd <|
        Task.perform
            StoreEventsToSend
        <|
            Task.map
                (\t -> List.singleton <| payload newUuid t { uuid = newUuid, posixtime = t, flow = Flow.Requested })
                Time.now


dispatchMany : Model -> List (EventBase -> Event) -> Effect Msg msg
dispatchMany model payloads =
    -- dispatch several events
    Effect.batch <| List.map (\payload -> dispatch model payload) payloads


identity : Model -> Maybe String
identity =
    .identity
