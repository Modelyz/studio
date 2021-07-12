module Page.Process exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import ES exposing (State, aggregate, getProcess)
import Html exposing (Html, a, br, button, div, i, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (decodeValue, errorToString)
import Json.Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Commitment as C exposing (Commitment)
import REA.Entity as Ent
import REA.Process exposing (Process)
import Random.Pcg.Extended as Random
import Status exposing (Status(..))
import Task
import Time exposing (millisToPosix, now, posixToMillis)


type Msg
    = NewEvent
    | NewCommitment
    | TimestampEvent ES.Event
    | EventsReceived Json.Encode.Value
    | EventStored Json.Encode.Value


type alias Model =
    { state : State
    , uuid : Uuid
    , process : Status Process
    }


init : Uuid -> State -> ( Model, Cmd Msg )
init uuid state =
    ( { state = state
      , uuid = uuid
      , process = Loading
      }
    , ES.getEvents <| Uuid.encode uuid
    )


timeCompare : Time.Posix -> Time.Posix -> Order
timeCompare t1 t2 =
    compare (posixToMillis t1) (posixToMillis t2)


syncModel : Model -> Model
syncModel model =
    { model
        | process =
            case getProcess model.state of
                Just p ->
                    Loaded p

                Nothing ->
                    Failed "Unknown error"
    }


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
                    ( { model | process = Failed (errorToString error) }
                    , Cmd.none
                    )

        NewCommitment ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.state.currentSeed

                ename =
                    "Commitment"

                -- TODO other types?
                event =
                    { uuid = newUuid
                    , posixtime = millisToPosix 0
                    , name = ename ++ " added"
                    , entityType = ename
                    , entity = Ent.Commitment (C.new newUuid)
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

        NewEvent ->
            ( model, Cmd.none )

        TimestampEvent event ->
            ( model, ES.storeEvent <| ES.encode event )

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
    { title = "Process"
    , body =
        [ viewNavbar
        , viewNotifications model
        , viewContent model
        ]
    }


viewNotifications : Model -> Html msg
viewNotifications model =
    let
        idbError =
            case model.process of
                Failed error ->
                    div
                        [ class "notification"
                        , class "is-warning"
                        ]
                        [ button [ class "delete" ] []
                        , text <| "Could not read events from IDB: " ++ error
                        ]

                _ ->
                    text ""
    in
    div
        [ class "container" ]
        [ idbError ]


viewContent : Model -> Html Msg
viewContent model =
    div []
        [ div [ class "section", class "hscroll-container" ]
            [ span [] [ text <| "Pizza sale # " ++ Uuid.toString model.uuid ] ]
        , div
            [ class "section", class "hscroll-container" ]
            [ div [ class "button", class "hscroll", onClick <| NewCommitment ] [ text "Order Pizza" ]
            , div [ class "button", class "hscroll", onClick <| NewCommitment ] [ text "Ask payment" ]
            , div [ class "button", class "hscroll", onClick <| NewEvent ] [ text "Receive Cash" ]
            , div [ class "button", class "hscroll", onClick <| NewEvent ] [ text "Deliver Pizza" ]
            ]
        , case model.process of
            Loaded p ->
                div [ class "columns is-multiline" ] <|
                    List.map viewThumbnail p.commitments

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


viewThumbnail : Commitment -> Html Msg
viewThumbnail c =
    div [ class "column is-one-quarter" ]
        [ a [ href <| "/commitment/" ++ Uuid.toString c.uuid ]
            [ div [ class "box" ]
                [ text <| c.name
                , br [] []
                , text <| Uuid.toString c.uuid
                ]
            ]
        ]
