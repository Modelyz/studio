port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, div, span, h1, img, button, a, nav)
import Html.Attributes exposing (class, src, width, href, attribute)
import Html.Events exposing (onClick)
import Json.Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid exposing (Uuid, generator)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import String
import Task
import Time
import Url
import Url.Parser


-- local imports
import ES
import Msg
import REA
import REA.Process
import REA.Commitment
import REA.CommitmentType
import REA.Event
import REA.ProcessType
import Route
import NotFound
import ErrorPage

---- MODEL ----

type alias Model =
    { currentSeed: Seed
    , currentUuid: Prng.Uuid.Uuid
    , url: Url.Url
    , route: Route.Route
    , navkey: Nav.Key
    , processtype: REA.ProcessType
    , processes: List REA.Process
    , posixtime: Time.Posix
    , events: List ES.Event
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
      , processtype=REA.ProcessType.new
      , processes=[]
      , posixtime=Time.millisToPosix 0
      , events=[]
      }
    , Cmd.none
    )


-- PORTS --

port storeEvent: Json.Encode.Value -> Cmd msg


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
                        , name="New " ++ ename ++ " added"
                        , entityType=ename
                        , entity=REA.COMMITMENTTYPE (REA.CommitmentType.new newUuid)
                        }
            in
                ( { model -- FIXME : update the relevant proces with new commitments (to do in agregate function)
                    | currentUuid = newUuid
                    , currentSeed = newSeed
                    , events = event :: model.events
                  }
                  , Task.perform Msg.TimestampEvent Time.now
                )
        Msg.NewEvent ->
            let
                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator model.currentSeed
                ename = "Event" -- TODO other types?
                event = { uuid=newUuid
                        , posixtime=model.posixtime
                        , name="New " ++ ename ++ " added"
                        , entityType=ename
                        , entity=REA.EVENT (REA.Event.new newUuid)
                        }
            in
                ( { model -- FIXME : update the relevant proces with new event (to do in agregate function)
                    | currentUuid = newUuid
                    , currentSeed = newSeed
                    , events = event :: model.events
                  }
                  , Task.perform Msg.TimestampEvent Time.now
                )
        Msg.NewProcess ->
            let
                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator model.currentSeed
                ename = "Process" -- TODO other types?
                event = { uuid=newUuid
                        , posixtime=model.posixtime
                        , name="New " ++ ename ++ " added"
                        , entityType=ename
                        , entity=REA.PROCESS (REA.Process.new newUuid)
                        }
            in
                ( { model
                    | processes=model.processes ++ [REA.Process.new newUuid]
                    , currentUuid = newUuid
                    , currentSeed = newSeed
                    , events = event :: model.events
                  }
                  , Task.perform Msg.TimestampEvent Time.now
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
                    in
                    ( { model
                      | events= timedEvent :: remaining
                      }
                    , storeEvent <| ES.encode timedEvent
                    )


---- VIEW ----


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
        , case model.route of
            Route.NotFound -> NotFound.document
            Route.Home -> 
                div
                    [ class "section"
                    ]
                    [ button
                        [ onClick <| Msg.NewProcess
                        , class "button"
                        ]
                        [ text "New pizza sale"
                        ]
                    , div [class "columns", class "is-multiline"]
                          <| List.map REA.Process.viewThumbnail model.processes
                    ]
            Route.SingleProcess uuid ->
                let
                    -- TODO move in the Process module
                    id = Prng.Uuid.fromString uuid
                    processes = case id of
                        Nothing -> []
                        Just i -> List.filter (\x -> (x.uuid == i)) model.processes
                in
                    case List.length processes  of
                        0 -> NotFound.document
                        1 -> case List.head model.processes of
                            Just p -> REA.Process.viewFullpage p
                            Nothing -> ErrorPage.document
                        _ -> ErrorPage.document
        ]
    }


onUrlRequest : Browser.UrlRequest -> Msg.Msg
onUrlRequest = Msg.LinkClicked


onUrlChange : Url.Url -> Msg.Msg
onUrlChange = Msg.UrlChanged


---- PROGRAM ----


main : Program (Int, List Int) Model Msg.Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
