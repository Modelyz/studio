module Shared exposing (Model, Msg(..), dispatch, dispatchMany, identity, init, update, uuidAggregator)

import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import IOStatus exposing (IOStatus(..))
import Json.Decode as Decode exposing (decodeString, decodeValue, errorToString)
import Json.Encode as Encode
import Message exposing (Message(..), Payload(..), exceptIC, getTime)
import MessageFlow exposing (MessageFlow(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Process
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Route exposing (Route)
import State exposing (State)
import Task
import Time exposing (millisToPosix, posixToMillis)
import Url exposing (Url)
import View.Style exposing (Menu(..), WindowSize, isMobile)
import Websocket as WS exposing (WSStatus(..), wsConnect, wsSend)


type alias Model =
    -- ui model related
    { version : Int
    , esversion : Int
    , currentSeed : Seed
    , route : Route
    , navkey : Nav.Key
    , windowSize : WindowSize
    , menu : Menu
    , admin : Bool

    -- ES and WS related
    , iostatus : IOStatus
    , wsstatus : WSStatus
    , timeoutReconnect : Int

    -- session related
    , identity : Maybe String
    , zone : Time.Zone
    , zonename : Time.ZoneName

    -- REA state related
    , state : State
    }


type Msg
    = WindowResized WindowSize
    | ToggleMenu
    | SwitchAdmin Bool
    | SetRoute Route
    | WSDisconnected Decode.Value
    | WSError Decode.Value
    | WSConnect ()
    | WSConnected Decode.Value
    | StoreMessagesToSend (List Message)
    | SendMessages (List Message)
    | MessagesStored Decode.Value
    | MessagesStoredTosend Decode.Value
    | MessagesRead Decode.Value
    | MessageSent Decode.Value
    | MessagesReceived String
    | GotZone Time.Zone
    | GotZoneName Time.ZoneName
    | GotNewSeed ( Int, List Int )


type alias Flags =
    { version : Int
    , esversion : Int
    , seed : Int
    , seedExtension : List Int
    , url : Maybe Url
    , windowSize : WindowSize
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map6 Flags
        (Decode.field "version" Decode.int)
        (Decode.field "esversion" Decode.int)
        (Decode.field "seed" Decode.int)
        (Decode.field "seedExtension" (Decode.list Decode.int))
        (Decode.field "url" Decode.string |> Decode.andThen (Url.fromString >> Decode.succeed))
        (Decode.field "windowSize"
            (Decode.map2 WindowSize (Decode.field "w" Decode.int) (Decode.field "h" Decode.int))
        )


init : Decode.Value -> Nav.Key -> ( Model, Cmd Msg )
init value navkey =
    case Decode.decodeValue flagsDecoder value of
        Ok f ->
            ( { version = f.version
              , esversion = f.esversion
              , currentSeed = initialSeed f.seed f.seedExtension
              , route = Maybe.map Route.toRoute f.url |> Maybe.withDefault Route.Home
              , navkey = navkey
              , windowSize = f.windowSize
              , menu =
                    if isMobile f.windowSize then
                        MobileClosed

                    else
                        Desktop
              , admin = True
              , iostatus = ESReading
              , wsstatus = WSClosed
              , timeoutReconnect = 1
              , identity = Nothing
              , state = State.empty
              , zone = Time.utc
              , zonename = Time.Offset 0
              }
            , Message.readMessages Encode.null
            )

        Err err ->
            ( { version = 0
              , esversion = 0
              , currentSeed = initialSeed 0 [ 0, 0 ]
              , route = Route.Home
              , navkey = navkey
              , windowSize = WindowSize 1024 768
              , admin = False
              , menu = Desktop
              , iostatus = IOError <| "Wrong init flags: " ++ errorToString err
              , wsstatus = WSClosed
              , timeoutReconnect = 1
              , identity = Nothing
              , state = State.empty
              , zone = Time.utc
              , zonename = Time.Offset 0
              }
            , Cmd.batch
                [ Task.perform GotZone Time.here
                , Task.perform GotZoneName Time.getZoneName
                , Message.readMessages Encode.null
                ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator model.currentSeed
    in
    case msg of
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

        SwitchAdmin b ->
            ( { model
                | admin = b
              }
            , Cmd.none
            )

        SetRoute route ->
            -- store the route to reload page init to the same route after reading messages
            ( { model | route = route }, Cmd.none )

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

        WSError str ->
            ( { model
                | iostatus =
                    decodeValue Decode.int str
                        |> Result.map (\_ -> IOIdle "Websocket changed its state")
                        |> Result.withDefault (IOError "Could not decode the readyState of the Websocket")
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
                            List.foldl State.aggregate model.state messages

                        lastMessageTime =
                            messages
                                |> List.map (getTime >> posixToMillis)
                                |> List.maximum
                                |> Maybe.withDefault 0
                                |> millisToPosix
                    in
                    ( { model
                        | iostatus = IOIdle "Just received messages"
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
                    ( { model | currentSeed = newSeed, iostatus = IOError <| "Error decoding messages: " ++ errorToString str }, Cmd.none )

        SendMessages messages ->
            -- send the new messages and the pending ones
            ( { model | iostatus = WSSending, currentSeed = newSeed }
            , WS.wsSend <|
                Encode.object <|
                    List.singleton <|
                        Tuple.pair "messages" <|
                            Encode.list Message.encode <|
                                Dict.values <|
                                    Dict.union (Dict.filter (\_ (Message meta _) -> meta.flow == Requested) model.state.pendingMessages) <|
                                        Dict.fromList <|
                                            List.map (\m -> ( Message.compare m, m )) <|
                                                messages
            )

        StoreMessagesToSend messages ->
            ( { model | currentSeed = newSeed, iostatus = ESStoring }
            , Message.storeMessagesToSend
                (Encode.object <|
                    List.singleton <|
                        Tuple.pair "messages" <|
                            Encode.list Message.encode messages
                )
            )

        MessagesStoredTosend messages ->
            case decodeValue (Decode.list Message.decoder) messages of
                Ok evs ->
                    if model.wsstatus == WSOpen then
                        ( { model | iostatus = ESReading }
                        , Cmd.batch
                            [ Message.readMessages Encode.null

                            -- send the new messages and the pending Requested ones
                            , wsSend <|
                                Encode.object <|
                                    List.singleton <|
                                        Tuple.pair "messages" <|
                                            Encode.list Message.encode <|
                                                Dict.values <|
                                                    Dict.union (Dict.filter (\_ (Message meta _) -> meta.flow == Requested) model.state.pendingMessages) <|
                                                        Dict.fromList <|
                                                            List.map (\m -> ( Message.compare m, m )) <|
                                                                evs
                            ]
                        )

                    else
                        ( { model | iostatus = IOIdle "Just stored messages to send" }, Message.readMessages Encode.null )

                Err err ->
                    ( { model | currentSeed = newSeed, iostatus = IOError <| "Error decoding stored messages: " ++ errorToString err }, Cmd.none )

        MessagesStored _ ->
            ( { model | iostatus = IOIdle "Just stored messages", currentSeed = newSeed }, Message.readMessages Encode.null )

        MessageSent letter ->
            case
                Result.map2 Tuple.pair
                    (decodeValue
                        (Decode.field "status" Decode.string
                            |> Decode.andThen
                                (\s ->
                                    if s == "OK" then
                                        Decode.succeed s

                                    else
                                        Decode.fail s
                                )
                        )
                        letter
                    )
                    (decodeValue (Decode.field "messages" <| Decode.list Message.decoder) letter)
            of
                Ok ( status, messages ) ->
                    if status == "OK" then
                        let
                            state =
                                model.state
                        in
                        ( { model | iostatus = IOIdle "Just sent messages", state = { state | pendingMessages = List.foldl (\m agg -> Dict.remove (Message.compare m) agg) state.pendingMessages messages } }
                        , Cmd.none
                        )

                    else
                        ( { model | iostatus = IOError status }, Cmd.none )

                Err err ->
                    ( { model | currentSeed = newSeed, iostatus = IOError <| "Error getting status of letter: " ++ errorToString err }, Cmd.none )

        MessagesReceived letter ->
            case decodeString (Decode.maybe <| Decode.field "messages" <| Decode.list Message.decoder) letter of
                Ok mbmessages ->
                    let
                        msgs =
                            Maybe.withDefault [] <| Maybe.map exceptIC mbmessages
                    in
                    ( { model
                        | wsstatus = WSOpen
                        , iostatus =
                            if List.length msgs > 0 then
                                ESStoring

                            else
                                IOIdle "Just received messages"
                      }
                    , if List.length msgs > 0 then
                        Message.storeMessages <|
                            Encode.object <|
                                List.singleton <|
                                    Tuple.pair "messages" <|
                                        Encode.list Message.encode <|
                                            Maybe.withDefault [] <|
                                                Maybe.map exceptIC mbmessages

                      else
                        Cmd.none
                    )

                Err err ->
                    ( { model | currentSeed = newSeed, iostatus = IOError <| "Error decoding received messages:\n" ++ errorToString err }, Cmd.none )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        GotZoneName zonename ->
            ( { model | zonename = zonename }, Cmd.none )

        GotNewSeed ( seed, seedExtension ) ->
            ( { model | currentSeed = initialSeed seed seedExtension }, Cmd.none )


initiateConnection : Uuid -> Model -> Cmd Msg
initiateConnection uuid model =
    Task.perform SendMessages <|
        Task.map
            (\t ->
                List.singleton <|
                    Message
                        { uuid = uuid, when = t, flow = Requested }
                        (InitiatedConnection
                            { lastMessageTime = model.state.lastMessageTime
                            , uuids = Dict.insert (Uuid.toString uuid) uuid model.state.uuids
                            }
                        )
            )
            Time.now


dispatch : Model -> Payload -> Effect Msg msg
dispatch model payload =
    -- take a Message payload and add the Metadata informations
    Effect.fromSharedCmd <|
        Task.perform
            StoreMessagesToSend
        <|
            Task.map
                (\time ->
                    let
                        ( newUuid, _ ) =
                            Random.step Uuid.generator model.currentSeed
                    in
                    List.singleton <| Message { uuid = newUuid, when = time, flow = Requested } payload
                )
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
                        List.reverse <|
                            List.foldl (uuidAggregator newSeed) [] <|
                                List.map (Message { uuid = newUuid, when = time, flow = Requested }) payloads
                )
                Time.now


identity : Model -> Maybe String
identity =
    .identity


uuidAggregator : Seed -> a -> List ( Uuid, Seed, a ) -> List ( Uuid, Seed, a )
uuidAggregator firstSeed i tuples =
    -- aggregator to generate several uuids from a list
    let
        newTuple =
            tuples
                |> List.head
                |> (\mt ->
                        case mt of
                            Just ( _, lastSeed, _ ) ->
                                let
                                    ( newUuid, newSeed ) =
                                        Random.step Uuid.generator lastSeed
                                in
                                ( newUuid, newSeed, i )

                            Nothing ->
                                let
                                    ( newUuid, newSeed ) =
                                        Random.step Uuid.generator firstSeed
                                in
                                ( newUuid, newSeed, i )
                   )
    in
    newTuple :: tuples


uuidMerger : List ( Uuid, Seed, Message ) -> List Message
uuidMerger tuples =
    -- merge the new uuids into the messages
    List.map
        (\( uuid, _, Message metadata payload ) ->
            Message { metadata | uuid = uuid } payload
        )
        tuples
