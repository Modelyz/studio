module Page.Process exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Event exposing (Event(..))
import EventFlow exposing (EventFlow(..))
import Html exposing (Html, a, br, div, nav, p, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import IOStatus exposing (IOStatus(..))
import Page.Navbar as Navbar
import Page.NotFound as NotFound
import Prng.Uuid as Uuid
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType exposing (CommitmentType)
import REA.Event as E exposing (Event)
import REA.EventType exposing (EventType)
import REA.Process exposing (Process)
import Route exposing (Route)
import Shared
import Spa.Page
import State exposing (getCommitmentTypes, getCommitments, getEventTypes, getEvents, getProcess)
import View exposing (View)


type alias Model =
    { route : Route
    , process : String
    }


type Msg
    = NewCommitment Process String
    | NewEvent Process String


type alias Flags =
    { route : Route
    , process : String
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Process p ->
            Just { route = route, process = p }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { route = flags.route
      , process = flags.process
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        NewCommitment process ctype ->
            let
                commitmentType =
                    shared.commitmentTypes
                        |> Set.toList
                        |> List.filter (\ct -> ct.name == ctype)
                        |> List.head
            in
            case commitmentType of
                Just ct ->
                    ( model
                    , Shared.dispatchT shared <|
                        \uuid t -> Event.CommitmentAdded { process = process, commitment = Commitment ct.name uuid t ct }
                    )

                Nothing ->
                    ( model
                    , Effect.none
                    )

        NewEvent process etype ->
            let
                eventType =
                    shared.eventTypes
                        |> Set.toList
                        |> List.filter (\et -> et.name == etype)
                        |> List.head
            in
            case eventType of
                Just et ->
                    ( model
                    , Shared.dispatchT shared <|
                        \uuid t -> Event.EventAdded { process = process, event = E.new et.name uuid t et }
                    )

                Nothing ->
                    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Process"
    , attributes = []
    , element =
        Html.div []
            [ Navbar.view shared model.route
            , viewContent shared model
            ]
    }


newCommitmentButton : Process -> CommitmentType -> Html Msg
newCommitmentButton process ct =
    div
        [ class "button"
        , class "hscroll"
        , onClick <| NewCommitment process ct.name
        ]
        [ text ct.name
        ]


newEventButton : Process -> EventType -> Html Msg
newEventButton process et =
    div
        [ class "button"
        , class "hscroll"
        , onClick <| NewEvent process et.name
        ]
        [ text et.name
        ]


viewContent : Shared.Model -> Model -> Html Msg
viewContent shared model =
    let
        process =
            getProcess shared model.process

        commitmentTypes =
            process
                |> Maybe.map .type_
                |> Maybe.map (getCommitmentTypes shared)
                |> Maybe.withDefault (Set.empty .name)

        eventTypes =
            process
                |> Maybe.map .type_
                |> Maybe.map (getEventTypes shared)
                |> Maybe.withDefault (Set.empty .name)
    in
    case process of
        Nothing ->
            NotFound.viewContent

        Just proc ->
            div []
                [ div
                    [ class "hero is-medium"
                    ]
                    [ div [ class "hero-body" ]
                        [ p [ class "title" ]
                            [ text <| proc.type_ ++ " # " ++ Uuid.toString proc.uuid
                            ]
                        ]
                    ]
                , div [ class "columns" ]
                    [ nav [ class "panel", style "margin" "0.5rem" ]
                        [ p [ class "panel-heading" ]
                            [ text "Commitments" ]
                        , div [ class "panel-block hscroll-container" ] <|
                            List.map
                                (newCommitmentButton proc)
                                (shared.commitmentTypes
                                    |> Set.filter (\ct -> Set.member ct commitmentTypes)
                                    |> Set.toList
                                )
                        , div [ class "panel-block" ]
                            [ div [ class "columns is-multiline" ]
                                (getCommitments shared proc
                                    |> Set.toList
                                    |> List.sortBy C.compare
                                    |> List.reverse
                                    |> List.map viewCommitmentThumbnail
                                )
                            ]
                        ]
                    , nav [ class "panel", style "margin" "0.5rem" ]
                        [ p [ class "panel-heading" ]
                            [ text "Events" ]
                        , div [ class "panel-block hscroll-container" ] <|
                            List.map
                                (newEventButton proc)
                                (shared.eventTypes
                                    |> Set.filter (\et -> Set.member et eventTypes)
                                    |> Set.toList
                                )
                        , div [ class "panel-block" ]
                            [ div [ class "columns is-multiline" ]
                                (getEvents shared proc
                                    |> Set.toList
                                    |> List.sortBy E.compare
                                    |> List.reverse
                                    |> List.map viewEventThumbnail
                                )
                            ]
                        ]
                    ]
                ]


viewCommitmentThumbnail : Commitment -> Html Msg
viewCommitmentThumbnail c =
    div [ class "column is-one-quarter" ]
        [ a
            [ href <| "/commitment/" ++ Uuid.toString c.uuid ]
            [ div
                [ class "card" ]
                [ text c.name
                , br [] []
                , text <| Uuid.toString c.uuid
                ]
            ]
        ]


viewEventThumbnail : Event -> Html Msg
viewEventThumbnail c =
    div [ class "column is-one-quarter" ]
        [ a
            [ href <| "/event/" ++ Uuid.toString c.uuid ]
            [ div
                [ class "card" ]
                [ text c.name
                , br [] []
                , text <| Uuid.toString c.uuid
                ]
            ]
        ]
