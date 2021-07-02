port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, div, span, h1, img, button, a, nav)
import Html.Attributes exposing (class, src, width, href, attribute)
import Html.Events exposing (onClick)
import Json.Encode
import Json.Decode
import Maybe exposing (Maybe(..))
import Prng.Uuid exposing (Uuid, generator)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import String
import Task
import Time exposing (millisToPosix, posixToMillis, now)
import Url
import Url.Parser


-- local imports
import ES
import Msg
import REA.Entity as En exposing (Entity)
import REA.Process as P exposing (Process)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Event as E
import REA.ProcessType as PT exposing (ProcessType)
import Route
import Page.Process
import Page.Processes
import NotFound
import ErrorPage

---- MODEL ----

type alias Model =
    { currentSeed: Seed
    , currentUuid: Prng.Uuid.Uuid
    , url: Url.Url
    , route: Route.Route
    , navkey: Nav.Key
    , processtype: ProcessType
    , processes: List Process
    , posixtime: Time.Posix
    , events: List ES.Event
    , readEventsError: Maybe String
    }


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg.Msg )
init ( seed, seedExtension ) url navkey =
    let
        ( newUuid, newSeed ) =
            step Prng.Uuid.generator <| initialSeed seed seedExtension
    in
    ( { currentSeed=initialSeed seed seedExtension
      , currentUuid=newUuid
      , navkey=navkey
      , url=url
      , route=Route.parseUrl url
      , processtype=PT.new
      , processes=[]
      , posixtime=millisToPosix 0
      , events=[]
      , readEventsError=Nothing
      }
    , getEvents Json.Encode.null
    )


-- PORTS --

port storeEvent: Json.Encode.Value -> Cmd msg
port getEvents: Json.Encode.Value -> Cmd msg
port receiveEvents: (Json.Encode.Value -> msg) -> Sub msg

---- UPDATE ----

update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.NoOp -> (model, Cmd.none)
        -- react to a click on a link
        Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                  ( { model
                    | route=Route.parseUrl url
                    }
                    , Nav.pushUrl model.navkey (Url.toString url) )
                Browser.External href ->
                  ( model, Nav.load href )
        -- react to an url change
        Msg.UrlChanged url ->
            ( { model
              | route = Route.parseUrl url
             }
            , Cmd.none
            )
        Msg.NewCommitment ->
            let
                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator model.currentSeed
                ename = "Commitment" -- TODO other types?
                event = { uuid=newUuid
                        , posixtime=model.posixtime
                        , name=ename ++ " added"
                        , entityType=ename
                        , entity=En.CommitmentType (CT.new newUuid)
                        }
            in
                ( { model -- FIXME : update the relevant proces with new commitments (to do in agregate function)
                    | currentUuid = newUuid
                    , currentSeed = newSeed
                    , events = event :: model.events
                  }
                  , Task.perform Msg.TimestampEvent now
                )
        Msg.NewEvent ->
            let
                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator model.currentSeed
                ename = "Event" -- TODO other types?
                event = { uuid=newUuid
                        , posixtime=model.posixtime
                        , name=ename ++ " added"
                        , entityType=ename
                        , entity=En.Event (E.new newUuid)
                        }
            in
                ( { model -- FIXME : update the relevant proces with new event (to do in agregate function)
                    | currentUuid = newUuid
                    , currentSeed = newSeed
                    , events = event :: model.events
                  }
                  , Task.perform Msg.TimestampEvent now
                )
        Msg.NewProcess ->
            let
                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator model.currentSeed
                ename = "Process" -- TODO other types?
                event = { uuid=newUuid
                        , posixtime=model.posixtime
                        , name=ename ++ " added"
                        , entityType=ename
                        , entity=En.Process (P.new newUuid)
                        }
            in
                ( { model
                    | processes=P.new newUuid :: model.processes
                    , currentUuid = newUuid
                    , currentSeed = newSeed
                    , events = event :: model.events
                  }
                  , Task.perform Msg.TimestampEvent now
                )
        -- add a timestamp to the event
        Msg.TimestampEvent time ->
            let
                first = List.head model.events
                remaining = case List.tail model.events of
                    Nothing -> []
                    Just r -> r
            in case first of
                Nothing -> (model, Cmd.none)
                Just event ->
                    let timedEvent = {event | posixtime=time}
                    in ( { model | events= timedEvent :: remaining }
                         , storeEvent <| ES.encode timedEvent
                       )
        Msg.EventsReceived results ->
            case Json.Decode.decodeValue (Json.Decode.list ES.decode) results of
                Ok events ->
                    let sortedEvents = List.sortWith (\e1 e2 -> timeCompare e2.posixtime e1.posixtime) events
                        updatedmodel = List.foldr aggregate model sortedEvents
                    in ( { updatedmodel
                          | events=sortedEvents}
                        , Cmd.none)
                Err error ->
                    ( { model
                      | readEventsError=Just (Json.Decode.errorToString error)
                    , events=[]} , Cmd.none)


timeSort : List Time.Posix -> List Time.Posix
timeSort times = List.sortWith timeCompare times


timeCompare : Time.Posix -> Time.Posix -> Order
timeCompare t1 t2 = compare (posixToMillis t1) (posixToMillis t2)
        


-- evolve the state given an event
aggregate: ES.Event -> Model -> Model
aggregate event model =
    case event.name of
        "Process added" -> -- TODO turn this into a type
            case En.toProcess event.entity of
                Nothing -> model
                Just p -> { model | processes = p :: model.processes }
        _ -> model


---- VIEW ----

viewNotifications : Model -> Html Msg.Msg
viewNotifications model = 
    let
        idbError = case model.readEventsError of
            Nothing ->
                text ""
            Just error ->
                div
                    [ class "notification"
                    , class "is-warning"
                    ]
                    [ button [class "delete"] []
                    , text <| "Could not read events from IDB: " ++ error
                    ]
    in
        div
            [class "container"]
            [idbError]
    


view : Model -> Browser.Document Msg.Msg
view model =
    { title = "Modelyz"
    , body =
        [ nav
            [ class "navbar"
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            ]
            [ div 
                [ class "navbar-brand"
                ]
                [ a
                    [ class "navbar-item"
                    , href "/"
                    ]
                    [ img
                        [ src "/static/logo.svg"
                        , width 50
                        ] []
                    ]
                , a
                    [ attribute "role" "button"
                    , class "navbar-burger"
                    , attribute "aria-label" "menu"
                    , attribute "aria-expanded" "false"
                    , attribute "dat-target" "navBar"
                    ]
                    [ span [ attribute "aria-hidden" "true" ] [ ]
                    , span [ attribute "aria-hidden" "true" ] [ ]
                    , span [ attribute "aria-hidden" "true" ] [ ]
                    ]
               ]
          ]
        , viewNotifications model
        , case model.route of
            Route.NotFound -> NotFound.document
            Route.Processes -> Page.Processes.view model.processes
            Route.SingleProcess uuid ->
                let
                    id = Prng.Uuid.fromString uuid
                    processes = case id of
                        Nothing -> []
                        Just i -> List.filter (\x -> (x.uuid == i)) model.processes
                in
                    case List.length processes  of
                        0 -> NotFound.document
                        1 -> case List.head model.processes of
                            Just p -> Page.Process.viewFullpage p
                            Nothing -> ErrorPage.document
                        _ -> ErrorPage.document
        ]
    }


onUrlRequest : Browser.UrlRequest -> Msg.Msg
onUrlRequest = Msg.LinkClicked


onUrlChange : Url.Url -> Msg.Msg
onUrlChange = Msg.UrlChanged


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg.Msg
subscriptions model =
    Sub.batch [
        receiveEvents Msg.EventsReceived
    ]


---- PROGRAM ----


main : Program (Int, List Int) Model Msg.Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
