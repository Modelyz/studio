port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import DictSet as Set
import Event exposing (Event(..), exceptCI, getTime)
import EventFlow exposing (EventFlow(..))
import IOStatus exposing (IOStatus(..))
import Json.Decode as Decode exposing (decodeString, decodeValue, errorToString)
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Msg exposing (Msg(..))
import Page.CommitmentTypes
import Page.EventTypes
import Page.NotFound
import Page.Process
import Page.ProcessType
import Page.ProcessTypes
import Page.Processes
import Prng.Uuid as Uuid exposing (generator)
import Process
import REA.Commitment as C
import REA.CommitmentType as CT
import REA.Event as E
import REA.EventType as ET
import Random.Pcg.Extended as Random exposing (initialSeed, step)
import Route exposing (parseUrl)
import State exposing (State, getProcess, getProcessType)
import Task
import Time exposing (millisToPosix, posixToMillis)
import Url exposing (Url)
import Websocket as WS exposing (WSStatus(..), wsConnect, wsSend)


type alias Model =
    State



-- TODO gather ports in a single typed one


port eventsReader : (Encode.Value -> msg) -> Sub msg


port eventsStored : (Encode.Value -> msg) -> Sub msg


port eventsStoredToSend : (Encode.Value -> msg) -> Sub msg


port wsSendStatus : (Encode.Value -> msg) -> Sub msg


port wsClose : (Encode.Value -> msg) -> Sub msg


port wsError : (Encode.Value -> msg) -> Sub msg


port wsOpened : (Encode.Value -> msg) -> Sub msg


port eventsReceiver : (String -> msg) -> Sub msg


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) url navkey =
    ( State.new (initialSeed seed seedExtension) navkey (parseUrl url)
    , Event.readEvents Encode.null
    )


initiateConnection : Uuid.Uuid -> Model -> Cmd Msg
initiateConnection uuid model =
    Task.perform SendEvents <|
        Task.map
            (\t ->
                List.singleton <|
                    ConnectionInitiated
                        { uuid = uuid
                        , posixtime = t
                        , flow = Requested
                        , lastEventTime = model.lastEventTime
                        , sessionUuid = uuid
                        , uuids = Set.insert uuid model.uuids
                        }
            )
            Time.now


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.None _ ->
            ( model, Cmd.none )

        Msg.WSConnect _ ->
            ( model, wsConnect () )

        Msg.WSConnected st ->
            let
                wsstatus =
                    WS.fromReadyState st

                timeoutReconnect =
                    if wsstatus == WSOpen then
                        max 1 <| remainderBy 4 (posixToMillis model.lastEventTime)

                    else
                        model.timeoutReconnect

                ( newUuid, _ ) =
                    Random.step Uuid.generator model.currentSeed

                cmd =
                    if wsstatus == WSOpen then
                        initiateConnection newUuid model

                    else
                        Cmd.none
            in
            ( { model | wsstatus = wsstatus, timeoutReconnect = timeoutReconnect }, cmd )

        Msg.WSError _ ->
            ( { model
                | iostatus =
                    IOError "Websocket error"
              }
            , Cmd.none
            )

        Msg.WSDisconnected _ ->
            ( { model
                | timeoutReconnect = min 30 (model.timeoutReconnect + 1)
                , wsstatus = WSClosed
              }
            , Task.perform Msg.WSConnect
                (Process.sleep (toFloat (1000 * model.timeoutReconnect)))
            )

        Msg.EventsRead results ->
            case decodeValue (Decode.list Event.decoder) results of
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

        Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navkey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- react to an url change
        Msg.UrlChanged url ->
            ( { model | route = parseUrl url }
            , Cmd.none
            )

        Msg.InputProcessName name ->
            let
                ptype =
                    model.inputProcessType
            in
            ( { model | inputProcessType = { ptype | name = name } }, Cmd.none )

        Msg.ProcessTypeChanged ptype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model
                | currentSeed = newSeed
                , inputProcessType = { name = "" }
              }
            , Task.perform StoreEventsToSend <|
                Task.map (\t -> List.singleton <| Event.ProcessTypeChanged { uuid = newUuid, posixtime = t, flow = Requested, ptype = ptype }) Time.now
            )

        Msg.DeleteProcessType ptype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model
                | currentSeed = newSeed
              }
            , Task.perform StoreEventsToSend <|
                Task.map (\t -> List.singleton <| ProcessTypeRemoved { uuid = newUuid, posixtime = t, flow = Requested, ptype = ptype.name }) Time.now
            )

        Msg.NewProcess ptype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model
                | currentSeed = newSeed
              }
            , Task.perform StoreEventsToSend <|
                Task.map (\t -> List.singleton <| ProcessAdded { uuid = newUuid, posixtime = t, flow = Requested, name = Uuid.toString newUuid, type_ = ptype.name }) Time.now
            )

        Msg.NewCommitment process ctype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed

                commitmentType =
                    model.commitmentTypes
                        |> Set.toList
                        |> List.filter (\ct -> ct.name == ctype)
                        |> List.head
            in
            case commitmentType of
                Just ct ->
                    ( { model
                        | currentSeed = newSeed
                      }
                    , Task.perform StoreEventsToSend <|
                        Task.map (\t -> List.singleton <| CommitmentAdded { uuid = newUuid, posixtime = t, flow = Requested, process = process, commitment = C.new ct.name newUuid t ct }) Time.now
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Msg.NewEventType ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model
                | inputEventType = ""
                , inputEventTypeProcessTypes = Set.empty identity
                , currentSeed = newSeed
              }
            , Task.perform StoreEventsToSend <|
                Task.map
                    (\t ->
                        EventTypeAdded { uuid = newUuid, posixtime = t, flow = Requested, eventType = ET.new model.inputEventType }
                            :: List.map
                                (\pt ->
                                    LinkedEventTypeToProcessType
                                        { uuid = newUuid, posixtime = t, flow = Requested, etype = model.inputEventType, ptype = pt }
                                )
                                (Set.toList model.inputEventTypeProcessTypes)
                    )
                    Time.now
            )

        Msg.NewCommitmentType name ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model
                | inputCommitmentType = ""
                , inputCommitmentTypeProcessTypes = Set.empty identity
                , currentSeed = newSeed
              }
            , Task.perform StoreEventsToSend <|
                Task.map
                    (\t ->
                        List.singleton <|
                            CommitmentTypeAdded
                                { uuid = newUuid, posixtime = t, flow = Requested, commitmentType = CT.new name }
                    )
                    Time.now
            )

        Msg.NewEvent process etype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed

                eventType =
                    model.eventTypes
                        |> Set.toList
                        |> List.filter (\et -> et.name == etype)
                        |> List.head
            in
            case eventType of
                Just et ->
                    ( { model
                        | currentSeed = newSeed
                      }
                    , Task.perform StoreEventsToSend <|
                        Task.map
                            (\t ->
                                List.singleton <|
                                    EventAdded
                                        { uuid = newUuid
                                        , posixtime = t
                                        , flow = Requested
                                        , process = process
                                        , event = E.new et.name newUuid t et
                                        }
                            )
                            Time.now
                    )

                Nothing ->
                    ( model, Cmd.none )

        Msg.SendEvents events ->
            -- send the new events and the pending ones
            ( { model | iostatus = WSSending }, WS.wsSend <| Encode.encode 0 <| Encode.list Event.encode <| Set.toList <| Set.union model.pendingEvents <| Set.fromList Event.compare events )

        Msg.StoreEventsToSend events ->
            ( { model | iostatus = ESStoring }, Event.storeEventsToSend (Encode.list Event.encode events) )

        Msg.EventsStoredTosend events ->
            case decodeValue (Decode.list Event.decoder) events of
                Ok evs ->
                    if model.wsstatus == WSOpen then
                        ( { model | iostatus = ESReading }
                        , Cmd.batch
                            [ Event.readEvents Encode.null

                            -- send the new events and the pending ones
                            , wsSend <| Encode.encode 0 <| Encode.list Event.encode <| Set.toList <| Set.union model.pendingEvents <| Set.fromList Event.compare evs
                            ]
                        )

                    else
                        ( { model | iostatus = IOIdle }, Event.readEvents Encode.null )

                Err err ->
                    ( { model | iostatus = IOError <| errorToString err }, Cmd.none )

        Msg.EventsStored _ ->
            ( { model
                | iostatus = IOIdle
              }
            , Event.readEvents Encode.null
            )

        Msg.EventsSent status ->
            case decodeValue Decode.string status of
                Ok str ->
                    if str == "OK" then
                        ( { model | iostatus = ESReading }, Cmd.none )

                    else
                        ( { model | iostatus = IOError str }, Cmd.none )

                Err err ->
                    ( { model | iostatus = IOError <| errorToString err }, Cmd.none )

        Msg.InputEventType etype ->
            ( { model | inputEventType = etype }, Cmd.none )

        Msg.DeleteEventType etype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model | currentSeed = newSeed }
            , Task.perform StoreEventsToSend <|
                Task.map (\t -> List.singleton <| EventTypeRemoved { uuid = newUuid, posixtime = t, flow = Requested, eventType = etype }) Time.now
            )

        Msg.InputCommitmentType ctype ->
            ( { model | inputCommitmentType = ctype }, Cmd.none )

        Msg.DeleteCommitmentType ctype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model | currentSeed = newSeed }
            , Task.perform StoreEventsToSend <|
                Task.map (\t -> List.singleton <| CommitmentTypeRemoved { uuid = newUuid, posixtime = t, flow = Requested, commitmentType = ctype }) Time.now
            )

        Msg.InputCommitmentTypeProcessType pt ->
            ( { model | inputCommitmentTypeProcessTypes = Set.insert pt model.inputCommitmentTypeProcessTypes }, Cmd.none )

        Msg.InputEventTypeProcessType pt ->
            ( { model | inputEventTypeProcessTypes = Set.insert pt model.inputEventTypeProcessTypes }, Cmd.none )

        Msg.EventsReceived ms ->
            case decodeString (Decode.list Event.decoder) ms of
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



-- TODO get or create a session uuid, retrieve the last event time, send to haskell )


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Route.NotFound ->
            Page.NotFound.view model

        Route.ProcessTypes ->
            Page.ProcessTypes.view model

        Route.ProcessType ptype ->
            getProcessType model ptype
                |> Maybe.map (Page.ProcessType.view model)
                |> Maybe.withDefault (Page.NotFound.view model)

        Route.Processes ptype_str ->
            Maybe.map (getProcessType model) ptype_str
                |> Maybe.andThen (\pt -> Maybe.map (Page.Processes.view model) pt)
                |> Maybe.withDefault (Page.NotFound.view model)

        Route.Process pname ->
            let
                process =
                    getProcess model pname
            in
            case process of
                Just p ->
                    Page.Process.view model p

                Nothing ->
                    Page.NotFound.view model

        Route.CommitmentTypes ->
            Page.CommitmentTypes.view model

        Route.EventTypes ->
            Page.EventTypes.view model


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    LinkClicked


onUrlChange : Url -> Msg
onUrlChange =
    UrlChanged


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ wsSendStatus Msg.EventsSent
        , eventsReader Msg.EventsRead
        , eventsStoredToSend Msg.EventsStoredTosend
        , eventsStored Msg.EventsStored
        , eventsReceiver Msg.EventsReceived
        , wsOpened Msg.WSConnected
        , wsClose Msg.WSDisconnected
        , wsError Msg.WSError
        ]


main : Program ( Int, List Int ) Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
