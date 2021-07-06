module Page.Process exposing (..)

import Browser exposing (Document)
import ES
import Html exposing (Html, a, br, button, div, h1, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Prng.Uuid exposing (Uuid, generator)
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Entity as Ent exposing (Entity)
import REA.Event as E exposing (Event)
import REA.Process exposing (Process)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time exposing (millisToPosix, now, posixToMillis)


type Msg
    = TimestampEvent ES.Event
    | EventsReceived Json.Encode.Value
    | NewEvent
    | NewCommitment


type Status a
    = Loading
    | Failed String
    | Loaded a


type alias Model =
    { session : Session
    , uuid : Uuid
    , process : Status Process
    }


init : Uuid -> Session -> ( Model, Cmd Msg )
init uuid session =
    ( { session = session
      , uuid = uuid
      , process = Loading
      }
    , ES.getEvents <| Prng.Uuid.encode uuid
      -- FIXME LOST!!
    )


timeSort : List Time.Posix -> List Time.Posix
timeSort times =
    List.sortWith timeCompare times


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
                    if p.uuid == model.uuid then
                        { model | process = Loaded p }

                    else
                        model

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EventsReceived results ->
            case Json.Decode.decodeValue (Json.Decode.list ES.decode) results of
                Ok events ->
                    let
                        sortedEvents =
                            List.sortWith (\e1 e2 -> timeCompare e2.posixtime e1.posixtime) events

                        modified =
                            List.foldr aggregate model sortedEvents
                    in
                    ( modified, Cmd.none )

                Err error ->
                    ( { model
                        | process = Failed (Json.Decode.errorToString error)
                      }
                    , Cmd.none
                    )

        TimestampEvent event ->
            ( model, ES.storeEvent <| ES.encode event )

        NewCommitment ->
            ( model, Cmd.none )

        NewEvent ->
            ( model, Cmd.none )


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


view : Model -> Browser.Document Msg
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
viewContent p =
    div [ class "section", class "hscroll-container" ]
        [ div [ class "button", class "hscroll", onClick <| NewCommitment ] [ text "Order Pizza" ]
        , div [ class "button", class "hscroll", onClick <| NewCommitment ] [ text "Ask payment" ]
        , div [ class "button", class "hscroll", onClick <| NewEvent ] [ text "Receive Cash" ]
        , div [ class "button", class "hscroll", onClick <| NewEvent ] [ text "Deliver Pizza" ]
        ]
