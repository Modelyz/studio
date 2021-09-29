port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import DictSet as Set
import ES exposing (Event(..), getProcess, getProcessType)
import Json.Decode as Decode exposing (decodeValue, errorToString)
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
import REA.Commitment as C
import REA.CommitmentType as CT
import REA.Event as E
import REA.EventType as ET
import REA.Process as P
import Random.Pcg.Extended as Random exposing (initialSeed, step)
import Route exposing (parseUrl)
import Status exposing (ESStatus(..), WSStatus(..))
import Task
import Time
import Url exposing (Url)


type alias Model =
    ES.State


port eventsReader : (Encode.Value -> msg) -> Sub msg


port eventsStored : (Encode.Value -> msg) -> Sub msg


port eventsReceiver : (Encode.Value -> msg) -> Sub msg


port sendStatus : (Encode.Value -> msg) -> Sub msg


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) url navkey =
    ( ES.new (initialSeed seed seedExtension) navkey (parseUrl url)
    , ES.readEvents Encode.null
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.None ->
            ( model, Cmd.none )

        Msg.EventsRead results ->
            case decodeValue (Decode.list ES.decoder) results of
                Ok events ->
                    let
                        emptymodel =
                            ES.new model.currentSeed model.navkey model.route
                    in
                    ( List.foldr ES.aggregate { emptymodel | esstatus = ESLoaded } (List.reverse events), Cmd.none )

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
              }
            , Task.perform StoreEvents <|
                Task.map (\t -> List.singleton <| ES.ProcessTypeChanged { uuid = newUuid, posixtime = t, ptype = ptype }) Time.now
            )

        Msg.DeleteProcessType ptype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model | currentSeed = newSeed }
            , Task.perform StoreEvents <|
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
            , Task.perform StoreEvents <|
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
            ( { model
                | currentSeed = newSeed
              }
            , case commitmentType of
                Just ct ->
                    Task.perform StoreEvents <|
                        Task.map (\t -> List.singleton <| CommitmentAdded { uuid = newUuid, posixtime = t, process = process, commitment = C.new ct.name newUuid t ct }) Time.now

                Nothing ->
                    Cmd.none
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
            , Task.perform StoreEvents <|
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
            , Task.perform StoreEvents <|
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
            ( { model
                | currentSeed = newSeed
              }
            , case eventType of
                Just et ->
                    Task.perform StoreEvents <|
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

                Nothing ->
                    Cmd.none
            )

        Msg.StoreEvents events ->
            ( model, ES.storeEvents (Encode.list ES.encode events) )

        Msg.EventsStored event ->
            ( model, ES.sendEvents <| Encode.encode 0 event )

        Msg.EventsSent status ->
            case decodeValue Decode.string status of
                Ok str ->
                    if str == "OK" then
                        ( model, ES.readEvents Encode.null )

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
            , Task.perform StoreEvents <|
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
            , Task.perform StoreEvents <|
                Task.map (\t -> List.singleton <| CommitmentTypeRemoved { uuid = newUuid, posixtime = t, commitmentType = ctype }) Time.now
            )

        Msg.InputCommitmentTypeProcessType pt ->
            ( { model | inputCommitmentTypeProcessTypes = Set.insert pt model.inputCommitmentTypeProcessTypes }, Cmd.none )

        Msg.InputEventTypeProcessType pt ->
            ( { model | inputEventTypeProcessTypes = Set.insert pt model.inputEventTypeProcessTypes }, Cmd.none )


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
        [ eventsStored Msg.EventsStored
        , eventsReader Msg.EventsRead
        , sendStatus Msg.EventsSent

        --        , eventsReceiver Msg.EventsReceived
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
