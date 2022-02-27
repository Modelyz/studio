port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import DictSet as Set
import ES exposing (Event(..), getProcess, getProcessType, getTime)
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
import REA.Process as P
import Random.Pcg.Extended as Random exposing (initialSeed, step)
import Result
import Route exposing (parseUrl)
import Status exposing (ESStatus(..), WSStatus(..))
import Task
import Time exposing (millisToPosix, posixToMillis)
import Url exposing (Url)


type alias Model =
    ES.State



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
    ( ES.new (initialSeed seed seedExtension) navkey (parseUrl url)
    , ES.readEvents Encode.null
    )


initiateConnection : Uuid.Uuid -> Model -> Cmd Msg
initiateConnection uuid model =
    Task.perform StoreEventsToSend <|
        Task.map
            (\t ->
                List.singleton <|
                    ConnectionInitiated
                        { uuid = uuid
                        , posixtime = t
                        , lastEventTime = model.lastEventTime
                        , sessionUuid = uuid
                        }
            )
            Time.now


wsReadyState2Status : Decode.Value -> WSStatus
wsReadyState2Status value =
    -- https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState
    let
        s =
            decodeValue Decode.int value
                |> Result.withDefault 9
    in
    case s of
        0 ->
            WSConnecting

        1 ->
            WSOnline

        2 ->
            WSDisconnecting

        3 ->
            WSOffline

        _ ->
            WSUnexpected


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.None ->
            ( model, Cmd.none )

        Msg.WSConnect _ ->
            ( { model | wsstatus = WSConnecting }, ES.wsConnect () )

        Msg.WSConnected st ->
            let
                wsstatus =
                    wsReadyState2Status st

                timeoutReconnect =
                    if wsstatus == WSOnline then
                        max 1 <| remainderBy 4 (posixToMillis model.lastEventTime)

                    else
                        model.timeoutReconnect

                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed

                cmd =
                    if wsstatus == WSOnline then
                        initiateConnection newUuid model

                    else
                        Cmd.none
            in
            ( { model | wsstatus = wsstatus, timeoutReconnect = timeoutReconnect }, cmd )

        Msg.WSError err ->
            ( { model
                | wsstatus = WSOffline
              }
            , Cmd.none
            )

        Msg.WSDisconnected err ->
            ( { model
                | timeoutReconnect = min 30 (model.timeoutReconnect + 1)
                , wsstatus = WSOffline
              }
            , Task.perform WSConnect (Process.sleep <| toFloat <| 1000 * model.timeoutReconnect)
            )

        Msg.EventsRead results ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            case decodeValue (Decode.list ES.decoder) results of
                Ok events ->
                    let
                        newmodel =
                            List.foldr ES.aggregate model (List.reverse events)
                    in
                    ( { newmodel
                        | esstatus = ESIdle
                        , wsstatus =
                            case model.wsstatus of
                                WSInit ->
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
                        WSInit ->
                            ES.wsConnect ()

                        _ ->
                            Cmd.none
                    )

                Err str ->
                    ( { model | esstatus = ESReadFailed (errorToString str) }, Cmd.none )

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
                Task.map (\t -> List.singleton <| ES.ProcessTypeChanged { uuid = newUuid, posixtime = t, ptype = ptype }) Time.now
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
                Task.map (\t -> List.singleton <| ProcessTypeRemoved { uuid = newUuid, posixtime = t, ptype = ptype.name }) Time.now
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
                Task.map (\t -> List.singleton <| ProcessAdded { uuid = newUuid, posixtime = t, type_ = ptype.name }) Time.now
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
                        Task.map (\t -> List.singleton <| CommitmentAdded { uuid = newUuid, posixtime = t, process = process, commitment = C.new ct.name newUuid t ct }) Time.now
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
                        EventTypeAdded
                            { uuid = newUuid
                            , posixtime = t
                            , eventType = ET.new model.inputEventType
                            }
                            :: List.map
                                (\pt ->
                                    LinkedEventTypeToProcessType
                                        { uuid = newUuid
                                        , posixtime = t
                                        , etype = model.inputEventType
                                        , ptype = pt
                                        }
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
                                { uuid = newUuid
                                , posixtime = t
                                , commitmentType = CT.new name
                                }
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
                                        , process = process
                                        , event = E.new et.name newUuid t et
                                        }
                            )
                            Time.now
                    )

                Nothing ->
                    ( model, Cmd.none )

        Msg.StoreEventsToSend events ->
            ( { model | esstatus = ESStoring }, ES.storeEventsToSend (Encode.list ES.encode events) )

        Msg.EventsStoredTosend events ->
            let
                storedEvents =
                    case decodeValue (Decode.list ES.decoder) events of
                        Ok es ->
                            es

                        Err _ ->
                            []
            in
            ( { model
                | esstatus = ESIdle
              }
            , ES.sendEvents <| Encode.encode 0 <| Encode.list ES.encode <| List.append (Set.toList model.pendingEvents) storedEvents
            )

        Msg.EventsStored events ->
            ( { model
                | esstatus = ESIdle
              }
            , ES.readEvents Encode.null
            )

        Msg.EventsSent status ->
            case decodeValue Decode.string status of
                Ok str ->
                    if str == "OK" then
                        ( { model | esstatus = ESReading }, ES.readEvents Encode.null )

                    else
                        ( { model | wsstatus = WSSendFailed str }, Cmd.none )

                Err err ->
                    ( { model | wsstatus = WSSendFailed <| errorToString err }, Cmd.none )

        Msg.InputEventType etype ->
            ( { model | inputEventType = etype }, Cmd.none )

        Msg.DeleteEventType etype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model | currentSeed = newSeed }
            , Task.perform StoreEventsToSend <|
                Task.map (\t -> List.singleton <| EventTypeRemoved { uuid = newUuid, posixtime = t, eventType = etype }) Time.now
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
                Task.map (\t -> List.singleton <| CommitmentTypeRemoved { uuid = newUuid, posixtime = t, commitmentType = ctype }) Time.now
            )

        Msg.InputCommitmentTypeProcessType pt ->
            ( { model | inputCommitmentTypeProcessTypes = Set.insert pt model.inputCommitmentTypeProcessTypes }, Cmd.none )

        Msg.InputEventTypeProcessType pt ->
            ( { model | inputEventTypeProcessTypes = Set.insert pt model.inputEventTypeProcessTypes }, Cmd.none )

        Msg.EventsReceived ms ->
            case decodeString (Decode.list ES.decoder) <| "[" ++ (ms |> String.trim |> String.split "\n" |> String.join ",") ++ "]" of
                Ok messages ->
                    ( { model | wsstatus = WSOnline, esstatus = ESStoring }
                    , ES.storeEvents <|
                        Encode.list ES.encode messages
                    )

                Err err ->
                    ( { model | wsstatus = WSReceiveFailed <| errorToString err }, Cmd.none )



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
