module Shared exposing (Model, Msg(..), dispatch, dispatchMany, dispatchT, identity, init, update, uuidAggregator)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import IOStatus exposing (IOStatus(..))
import Json.Decode as Decode exposing (decodeString, decodeValue, errorToString)
import Json.Encode as Encode
import Message exposing (Message(..), Metadata, Payload(..), exceptCI, getTime)
import MessageFlow as Flow
import Prng.Uuid as Uuid exposing (Uuid)
import Process
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Route exposing (Route(..), toString)
import State exposing (State)
import Task
import Time exposing (millisToPosix, posixToMillis)
import Url as Url exposing (Url)
import Url.Parser as Parser
import View.Style as Style exposing (Menu(..), WindowSize, isMobile)
import Websocket as WS exposing (WSStatus(..), wsConnect, wsSend)


type alias Model =
    -- ui model related
    { currentSeed : Seed
    , route : Route
    , navkey : Nav.Key
    , windowSize : WindowSize
    , menu : Menu

    -- ES and WS related
    , iostatus : IOStatus
    , wsstatus : WSStatus
    , timeoutReconnect : Int

    -- session related
    , identity : Maybe String

    -- REA state related
    , state : State
    }


type Msg
    = None ()
    | WindowResized WindowSize
    | ToggleMenu
    | SetRoute Route
    | PushRoute Route
    | ReplaceRoute Route
    | WSDisconnected Decode.Value
    | WSError Decode.Value
    | WSConnect ()
    | WSConnected Decode.Value
    | StoreMessagesToSend (List Message)
    | SendMessages (List Message)
    | MessagesStored Decode.Value
    | MessagesStoredTosend Decode.Value
    | MessagesRead Decode.Value
    | MessagesSent Decode.Value
    | MessagesReceived String


type alias Flags =
    { seed : Int
    , seedExtension : List Int
    , url : Maybe Url
    , windowSize : WindowSize
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map4 Flags
        (Decode.field "seed" Decode.int)
        (Decode.field "seedExtension" (Decode.list Decode.int))
        (Decode.field "url" Decode.string |> Decode.andThen (\u -> Url.fromString u |> Decode.succeed))
        (Decode.field "windowSize"
            (Decode.map2 WindowSize (Decode.field "w" Decode.int) (Decode.field "h" Decode.int))
        )


init : Decode.Value -> Nav.Key -> ( Model, Cmd Msg )
init value navkey =
    case Decode.decodeValue flagsDecoder value of
        Ok f ->
            ( { currentSeed = initialSeed f.seed f.seedExtension
              , route = Maybe.map Route.toRoute f.url |> Maybe.withDefault Home
              , navkey = navkey
              , windowSize = f.windowSize
              , menu =
                    if isMobile f.windowSize then
                        MobileClosed

                    else
                        Desktop
              , iostatus = ESReading
              , wsstatus = WSClosed
              , timeoutReconnect = 1
              , identity = Nothing
              , state = State.empty
              }
            , Message.readMessages Encode.null
            )

        Err err ->
            ( { currentSeed = initialSeed 0 [ 0, 0 ]
              , route = Route.Home
              , navkey = navkey
              , windowSize = WindowSize 1024 768
              , menu = Desktop
              , iostatus = IOError <| "Wrong init flags: " ++ errorToString err
              , wsstatus = WSClosed
              , timeoutReconnect = 1
              , identity = Nothing
              , state = State.empty
              }
            , Message.readMessages Encode.null
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator model.currentSeed
    in
    case msg of
        None _ ->
            ( model, Cmd.none )

        WindowResized size ->
            ( { model
                | windowSize = size
                , menu =
                    if isMobile size then
                        MobileClosed

                    else
                        Desktop
              }
            , Cmd.none
            )

        ToggleMenu ->
            ( { model
                | menu =
                    case model.menu of
                        Desktop ->
                            Desktop

                        MobileOpen ->
                            MobileClosed

                        MobileClosed ->
                            MobileOpen
              }
            , Cmd.none
            )

        SetRoute route ->
            -- store the route to reload page init to the same route after reading messages
            ( { model | route = route }, Cmd.none )

        PushRoute route ->
            ( model, Nav.pushUrl model.navkey <| Route.toString route )

        ReplaceRoute route ->
            ( model, Nav.replaceUrl model.navkey <| Route.toString route )

        WSConnect _ ->
            ( model, wsConnect () )

        WSConnected st ->
            let
                wsstatus =
                    WS.fromReadyState st

                timeoutReconnect =
                    if wsstatus == WSOpen then
                        max 1 <| remainderBy 4 (posixToMillis model.state.lastMessageTime)

                    else
                        model.timeoutReconnect

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
                , currentSeed = newSeed
                , wsstatus = WSClosed
              }
            , Task.perform WSConnect
                (Process.sleep (toFloat (1000 * model.timeoutReconnect)))
            )

        MessagesRead results ->
            case decodeValue (Decode.list Message.decoder) results of
                Ok messages ->
                    let
                        newstate =
                            List.foldr State.aggregate model.state (List.reverse messages)

                        lastMessageTime =
                            messages
                                |> List.map (getTime >> posixToMillis)
                                |> List.maximum
                                |> Maybe.withDefault 0
                                |> millisToPosix
                    in
                    ( { model
                        | iostatus = IOIdle
                        , wsstatus =
                            case model.wsstatus of
                                WSClosed ->
                                    WSConnecting

                                _ ->
                                    model.wsstatus
                        , state = { newstate | lastMessageTime = lastMessageTime }
                      }
                    , Cmd.batch
                        [ case model.wsstatus of
                            WSClosed ->
                                wsConnect ()

                            _ ->
                                Cmd.none

                        -- trigger a page init so that the page init build its local model
                        -- after the shared state has been loaded from indexeddb
                        , Nav.pushUrl model.navkey <| Route.toString model.route
                        ]
                    )

                Err str ->
                    ( { model | currentSeed = newSeed, iostatus = IOError <| errorToString str }, Cmd.none )

        SendMessages messages ->
            -- send the new messages and the pending ones
            ( { model | iostatus = WSSending, currentSeed = newSeed }
            , WS.wsSend <|
                Encode.encode 0 <|
                    Encode.list Message.encode <|
                        Set.toList <|
                            Set.union model.state.pendingMessages <|
                                Set.fromList Message.compare messages
            )

        StoreMessagesToSend messages ->
            ( { model | currentSeed = newSeed, iostatus = ESStoring }, Message.storeMessagesToSend (Encode.list Message.encode messages) )

        MessagesStoredTosend messages ->
            case decodeValue (Decode.list Message.decoder) messages of
                Ok evs ->
                    if model.wsstatus == WSOpen then
                        ( { model | iostatus = ESReading }
                        , Cmd.batch
                            [ Message.readMessages Encode.null

                            -- send the new messages and the pending ones
                            , wsSend <|
                                Encode.encode 0 <|
                                    Encode.list Message.encode <|
                                        Set.toList <|
                                            Set.union model.state.pendingMessages <|
                                                Set.fromList Message.compare evs
                            ]
                        )

                    else
                        ( { model | iostatus = IOIdle }, Message.readMessages Encode.null )

                Err err ->
                    ( { model | currentSeed = newSeed, iostatus = IOError <| errorToString err }, Cmd.none )

        MessagesStored _ ->
            ( { model | iostatus = IOIdle, currentSeed = newSeed }, Message.readMessages Encode.null )

        MessagesSent status ->
            case decodeValue Decode.string status of
                Ok str ->
                    if str == "OK" then
                        ( { model | iostatus = ESReading }, Cmd.none )

                    else
                        ( { model | iostatus = IOError str }, Cmd.none )

                Err err ->
                    ( { model | currentSeed = newSeed, iostatus = IOError <| errorToString err }, Cmd.none )

        MessagesReceived ms ->
            case decodeString (Decode.list Message.decoder) ms of
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
                        Message.storeMessages <|
                            Encode.list Message.encode <|
                                exceptCI messages

                      else
                        Cmd.none
                    )

                Err err ->
                    ( { model | currentSeed = newSeed, iostatus = IOError <| errorToString err }, Cmd.none )


initiateConnection : Uuid -> Model -> Cmd Msg
initiateConnection uuid model =
    Task.perform SendMessages <|
        Task.map
            (\t ->
                List.singleton <|
                    Message
                        { uuid = uuid, when = t, flow = Flow.Requested }
                        (ConnectionInitiated
                            { lastMessageTime = model.state.lastMessageTime
                            , uuids = Set.insert uuid model.state.uuids
                            }
                        )
            )
            Time.now


dispatch : Model -> Payload -> Effect Msg msg
dispatch model payload =
    -- take a Message payload and add the Metadata informations
    let
        ( newUuid, _ ) =
            Random.step Uuid.generator model.currentSeed
    in
    Effect.fromSharedCmd <|
        Task.perform
            StoreMessagesToSend
        <|
            Task.map
                (\time -> List.singleton <| Message { uuid = newUuid, when = time, flow = Flow.Requested } payload)
                Time.now


dispatchT : Model -> (Uuid -> Time.Posix -> Payload) -> Effect Msg msg
dispatchT model newPayload =
    -- take a Message payload, feed with a uuid and posixtime, and add the Metadata informations
    let
        ( newUuid, _ ) =
            Random.step Uuid.generator model.currentSeed
    in
    Effect.fromSharedCmd <|
        Task.perform StoreMessagesToSend <|
            Task.map
                (\t -> List.singleton <| Message { uuid = newUuid, when = t, flow = Flow.Requested } (newPayload newUuid t))
                Time.now


dispatchMany : Model -> List Payload -> Effect Msg msg
dispatchMany model payloads =
    -- dispatch several messages
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator model.currentSeed
    in
    Effect.fromSharedCmd <|
        Task.perform StoreMessagesToSend <|
            Task.map
                (\time ->
                    uuidMerger <|
                        List.foldl (uuidAggregator newSeed) [] <|
                            List.map (Message { uuid = newUuid, when = time, flow = Flow.Requested }) payloads
                )
                Time.now


identity : Model -> Maybe String
identity =
    .identity


uuidAggregator : Seed -> a -> List ( ( Uuid, Seed ), a ) -> List ( ( Uuid, Seed ), a )
uuidAggregator firstSeed i tuples =
    -- aggregator to generate several uuids from a list
    let
        newTuple =
            tuples
                |> List.head
                |> (\mt ->
                        case mt of
                            Just t ->
                                let
                                    ( lastUuid, lastSeed ) =
                                        Tuple.first t

                                    ( newUuid, newSeed ) =
                                        Random.step Uuid.generator lastSeed
                                in
                                ( ( newUuid, newSeed ), i )

                            Nothing ->
                                let
                                    ( newUuid, newSeed ) =
                                        Random.step Uuid.generator firstSeed
                                in
                                ( ( newUuid, newSeed ), i )
                   )
    in
    newTuple :: tuples


uuidMerger : List ( ( Uuid, Seed ), Message ) -> List Message
uuidMerger tuples =
    -- merge the new uuids into the messages
    List.map
        (\t ->
            let
                ( uuid, _ ) =
                    Tuple.first t

                message =
                    Tuple.second t
            in
            case message of
                Message metadata payload ->
                    Message { metadata | uuid = uuid } payload
        )
        tuples
