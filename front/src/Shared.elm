module Shared exposing (Model, Msg(..), dispatch, dispatchMany, dispatchT, findEntityType, identity, init, isChild, isChildOfAny, restrictBy, update)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Event exposing (Event(..), EventBase, EventPayload(..), exceptCI, getTime)
import EventFlow as Flow
import IOStatus exposing (IOStatus(..))
import Json.Decode as Json exposing (decodeString, decodeValue, errorToString)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Process
import REA.EntityType as ENT exposing (EntityType)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Route exposing (Route(..), toString)
import State exposing (State)
import Style exposing (Menu(..), WindowSize, isMobile)
import Task
import Time exposing (millisToPosix, posixToMillis)
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
    | WSDisconnected Json.Value
    | WSError Json.Value
    | WSConnect ()
    | WSConnected Json.Value
    | StoreEventsToSend (List Event.Event)
    | SendEvents (List Event.Event)
    | EventsStored Json.Value
    | EventsStoredTosend Json.Value
    | EventsRead Json.Value
    | EventsSent Json.Value
    | EventsReceived String


type alias Flags =
    { seed : Int
    , seedExtension : List Int
    , windowSize : WindowSize
    }


flagsDecoder : Json.Decoder Flags
flagsDecoder =
    Json.map3 Flags
        (Json.field "seed" Json.int)
        (Json.field "seedExtension" (Json.list Json.int))
        (Json.field "windowSize"
            (Json.map2 WindowSize (Json.field "w" Json.int) (Json.field "h" Json.int))
        )


init : Json.Value -> Nav.Key -> ( Model, Cmd Msg )
init value navkey =
    case Json.decodeValue flagsDecoder value of
        Ok f ->
            ( { currentSeed = initialSeed f.seed f.seedExtension
              , route = Route.Home
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
            , Event.readEvents Encode.null
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
            , Event.readEvents Encode.null
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            -- store the route to reload page init to the same route after reading events
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
                        max 1 <| remainderBy 4 (posixToMillis model.state.lastEventTime)

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
                        newstate =
                            List.foldr State.aggregate model.state (List.reverse events)

                        lastEventTime =
                            events
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
                        , state = { newstate | lastEventTime = lastEventTime }
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
                    ( { model | iostatus = IOError <| errorToString str }, Cmd.none )

        SendEvents events ->
            -- send the new events and the pending ones
            ( { model | iostatus = WSSending }
            , WS.wsSend <|
                Encode.encode 0 <|
                    Encode.list Event.encode <|
                        Set.toList <|
                            Set.union model.state.pendingEvents <|
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
                                            Set.union model.state.pendingEvents <|
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
                    Event
                        { uuid = uuid, when = t, flow = Flow.Requested }
                        (ConnectionInitiated
                            { lastEventTime = model.state.lastEventTime
                            , uuids = Set.insert uuid model.state.uuids
                            }
                        )
            )
            Time.now


dispatch : Model -> EventPayload -> Effect Msg msg
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
                (\t -> List.singleton <| Event { uuid = newUuid, when = t, flow = Flow.Requested } payload)
                Time.now


dispatchT : Model -> (Uuid -> Time.Posix -> EventPayload) -> Effect Msg msg
dispatchT model newPayload =
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
                (\t -> List.singleton <| Event { uuid = newUuid, when = t, flow = Flow.Requested } (newPayload newUuid t))
                Time.now


dispatchMany : Model -> List EventPayload -> Effect Msg msg
dispatchMany model payloads =
    -- dispatch several events
    Effect.batch <| List.map (dispatch model) payloads


identity : Model -> Maybe String
identity =
    .identity


restrictBy : Model -> EntityType -> DictSet String EntityType -> DictSet String EntityType
restrictBy s scope ets =
    -- keep only the entityTypes which are children of the entityTypes of the scopes which are children of the given scope
    let
        parentTypes =
            s.state.restrictions
                |> Set.map ENT.compare .scope
                |> Set.filter (isChild s scope)
    in
    ets |> Set.filter (isChildOfAny s parentTypes)


isChildOfAny : Model -> DictSet String EntityType -> EntityType -> Bool
isChildOfAny s ets et =
    -- the entity type must be a child of one of the entity types of the identifier type
    if Set.isEmpty ets then
        -- identifier valid for all entity types
        True

    else
        ets |> Set.toList |> List.any (isParent s et)


isParent : Model -> EntityType -> EntityType -> Bool
isParent s child item =
    -- true if item is parent of the child
    if child == item then
        True

    else
        ENT.toType child
            |> .parent
            |> Maybe.andThen (\et -> findEntityType et s.state.entityTypes)
            |> Maybe.map (\x -> isParent s x item)
            |> Maybe.withDefault False


isChild : Model -> EntityType -> EntityType -> Bool
isChild s parent item =
    -- true if item is a child of parent
    isChild s item parent


findEntityType : String -> DictSet String EntityType -> Maybe EntityType
findEntityType name ets =
    Set.filter (\et -> ENT.toName et == name) ets
        |> Set.toList
        |> List.head
