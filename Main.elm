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
import REA.ProcessType
import Route
import NotFound

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
        -- create a new event
        Msg.NewEvent ->
            let
                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator model.currentSeed
                event = { uuid=newUuid
                        , posixtime=model.posixtime
                        , name="New " ++ "Process" ++ " added"
                        , entityType="Process" -- TODO other types?
                        , entity=REA.PROCESS (REA.Process.new newUuid)
                        }
            in
                ( { model
                    | processes=model.processes ++ [ newEvent newUuid ]
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


newEvent : Prng.Uuid.Uuid -> REA.Process
newEvent id =
    { uuid=id
    , name="Pizza Sale #" ++ Prng.Uuid.toString id
    , contract=
            { ctype=
                REA.ContractType
                { name="Sale"
                , ctype=Nothing
                }
            , name="Pizza sale #" ++ Prng.Uuid.toString id
            , parties=[]
            }
--    , commitments=[]
--    , events=[]
    }


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
                        [ onClick Msg.NewEvent
                        , class "button"
                        ]
                        [ text "New pizza sale"
                        ]
                    , div [class "columns", class "is-multiline"]
                          <| List.map REA.Process.view model.processes
                    ]
            Route.SingleProcess uuid ->
                div [ ]
                    [ text <| "process" ++ uuid ]
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
