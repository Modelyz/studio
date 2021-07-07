module Page.Processes exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import ES
import Html exposing (Html, a, br, button, div, i, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (decodeValue, errorToString)
import Json.Encode
import Prng.Uuid exposing (generator)
import REA.Entity as Ent
import REA.Process as P exposing (Process)
import Random.Pcg.Extended exposing (step)
import Session exposing (Session)
import Task
import Time exposing (millisToPosix, now, posixToMillis)


type Msg
    = NewProcess
    | TimestampEvent ES.Event
    | EventsReceived Json.Encode.Value
    | EventStored Json.Encode.Value


type Status a
    = Loading
    | Failed String
    | Loaded a


type alias Model =
    { session : Session
    , processes : Status (List Process)
    }


timeCompare : Time.Posix -> Time.Posix -> Order
timeCompare t1 t2 =
    compare (posixToMillis t1) (posixToMillis t2)



--- evolve the state given an event


aggregate : ES.Event -> Model -> Model
aggregate event model =
    case event.name of
        "Process added" ->
            -- TODO turn this into a type
            case Ent.toProcess event.entity of
                Nothing ->
                    model

                Just p ->
                    case model.processes of
                        Loaded ps ->
                            { model | processes = Loaded (p :: ps) }

                        _ ->
                            { model | processes = Loaded [ p ] }

        _ ->
            model


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , processes = Loaded []
      }
    , ES.getEvents Json.Encode.null
      --  andThen ... redirect
      --    , getSnapshot
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimestampEvent event ->
            ( model, ES.encode event |> ES.storeEvent )

        EventStored _ ->
            ( model, ES.getEvents Json.Encode.null )

        EventsReceived results ->
            case decodeValue (Json.Decode.list ES.decode) results of
                Ok events ->
                    let
                        sortedEvents =
                            List.sortWith (\e1 e2 -> timeCompare e2.posixtime e1.posixtime) events

                        updatedmodel =
                            List.foldr aggregate { model | processes = Loaded [] } sortedEvents
                    in
                    ( updatedmodel, Cmd.none )

                Err error ->
                    ( { model | processes = Failed (errorToString error) }
                    , Cmd.none
                    )

        NewProcess ->
            let
                ( newUuid, newSeed ) =
                    step generator model.session.currentSeed

                ename =
                    "Process"

                -- TODO other types?
                event =
                    { uuid = newUuid
                    , posixtime = millisToPosix 0
                    , name = ename ++ " added"
                    , entityType = ename
                    , entity = Ent.Process (P.new newUuid)
                    }
            in
            ( { model
                | session =
                    let
                        s =
                            model.session
                    in
                    { s
                        | currentUuid = newUuid
                        , currentSeed = newSeed
                    }
              }
            , Task.perform TimestampEvent <|
                Task.map (\t -> { event | posixtime = t }) now
            )


viewNavbar : Html Msg
viewNavbar =
    nav
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
                    ]
                    []
                ]
            , a
                [ attribute "role" "button"
                , class "navbar-burger"
                , attribute "aria-label" "menu"
                , attribute "aria-expanded" "false"
                , attribute "dat-target" "navBar"
                ]
                [ span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "Processes"
    , body =
        [ viewNavbar
        , viewContent model
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div
        [ class "section"
        ]
        [ button
            [ onClick <| NewProcess
            , class "button"
            ]
            [ text "New pizza sale"
            ]
        , case model.processes of
            Loaded ps ->
                div [ class "columns is-multiline" ] <|
                    List.map viewThumbnail ps

            Loading ->
                div [ class "section" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-spinner fa-pulse" ]
                                []
                            ]
                        ]
                    , span [] [ text " Loading..." ]
                    ]

            Failed error ->
                div [ class "section" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-bug" ]
                                []
                            ]
                        ]
                    , span [] [ text <| " Error : " ++ error ]
                    ]
        ]


viewThumbnail : Process -> Html Msg
viewThumbnail p =
    div [ class "column is-one-quarter" ]
        [ a [ href <| "/process/" ++ Prng.Uuid.toString p.uuid ]
            [ div [ class "box" ]
                [ text <| p.name
                , br [] []
                , text <| Prng.Uuid.toString p.uuid
                ]
            ]
        ]
