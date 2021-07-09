module Page.Processes exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import ES exposing (State, aggregate)
import Html exposing (Html, a, br, button, div, i, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (decodeValue, errorToString)
import Json.Encode
import Prng.Uuid as Uuid
import REA.Entity as Ent
import REA.Process as P exposing (Process)
import Random.Pcg.Extended as Random
import Status exposing (Status(..))
import Task
import Time exposing (millisToPosix, now, posixToMillis)


type Msg
    = NewProcess
    | TimestampEvent ES.Event
    | EventsReceived Json.Encode.Value
    | EventStored Json.Encode.Value


type alias Model =
    { state : ES.State
    , processes : Status (List Process)
    }


timeCompare : Time.Posix -> Time.Posix -> Order
timeCompare t1 t2 =
    compare (posixToMillis t1) (posixToMillis t2)


init : ES.State -> ( Model, Cmd Msg )
init state =
    ( { state = state
      , processes = Loading
      }
    , ES.getEvents Json.Encode.null
      --    , getSnapshot
    )


syncModel : Model -> Model
syncModel model =
    { model | processes = Loaded model.state.processes }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EventsReceived results ->
            case decodeValue (Json.Decode.list ES.decode) results of
                Ok events ->
                    let
                        sortedEvents =
                            List.sortWith (\e1 e2 -> timeCompare e2.posixtime e1.posixtime) events

                        state =
                            model.state

                        newState =
                            List.foldr aggregate { state | processes = [] } sortedEvents

                        newmodel =
                            syncModel { model | state = newState }
                    in
                    ( newmodel, Cmd.none )

                Err error ->
                    ( { model | processes = Failed (errorToString error) }
                    , Cmd.none
                    )

        NewProcess ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.state.currentSeed

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
                | state =
                    let
                        s =
                            model.state
                    in
                    { s
                        | currentUuid = newUuid
                        , currentSeed = newSeed
                    }
              }
            , Task.perform TimestampEvent <|
                Task.map (\t -> { event | posixtime = t }) now
            )

        TimestampEvent event ->
            ( model, ES.encode event |> ES.storeEvent )

        EventStored _ ->
            ( model, ES.getEvents Json.Encode.null )


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
        [ a [ href <| "/process/" ++ Uuid.toString p.uuid ]
            [ div [ class "box" ]
                [ text <| p.name
                , br [] []
                , text <| Uuid.toString p.uuid
                ]
            ]
        ]
