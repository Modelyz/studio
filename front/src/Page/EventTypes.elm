module Page.EventTypes exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Event
import Html exposing (Html, button, div, form, h1, input, label, p, span, text)
import Html.Attributes exposing (checked, class, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Page.Navbar as Navbar
import REA.EventType as ET exposing (EventType)
import REA.ProcessType as PT
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    { route : Route
    , inputEventType : String
    , inputEventTypeProcessTypes : DictSet String String
    }


type Msg
    = DeleteEventType EventType
    | InputEventType String
    | NewEventType
    | InputEventTypeProcessType String


type alias Flags =
    { route : Route
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.EventTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init _ flags =
    ( { route = flags.route
      , inputEventType = ""
      , inputEventTypeProcessTypes = Set.empty identity
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        NewEventType ->
            ( { model
                | inputEventType = ""
                , inputEventTypeProcessTypes = Set.empty identity
              }
            , Shared.dispatchMany shared <|
                Event.EventTypeAdded { eventType = ET.new model.inputEventType }
                    :: List.map (\pt -> Event.LinkedEventTypeToProcessType { etype = model.inputEventType, ptype = pt }) (Set.toList model.inputEventTypeProcessTypes)
            )

        InputEventType etype ->
            ( { model | inputEventType = etype }, Effect.none )

        DeleteEventType etype ->
            ( model
            , Shared.dispatch shared <| Event.EventTypeRemoved { eventType = etype }
            )

        InputEventTypeProcessType pt ->
            ( { model | inputEventTypeProcessTypes = Set.insert pt model.inputEventTypeProcessTypes }, Effect.none )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Event Types"
    , attributes = []
    , element =
        Html.div []
            [ Navbar.view shared model.route
            , viewContent shared model
            ]
    }


viewThumbnail : EventType -> Html Msg
viewThumbnail et =
    div
        [ class "container"
        , style "background" "yellow"
        ]
        [ div
            [ class "box", id et.name ]
            [ text et.name
            , button
                [ class "delete is-medium"
                , onClick <| DeleteEventType et
                ]
                []
            ]
        ]


viewContent : Shared.Model -> Model -> Html Msg
viewContent shared model =
    div
        []
        [ div
            [ class "hero is-medium"
            ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text "Event Types"
                    ]
                , p [ class "subtitle" ] [ text "What kind of events may have occured in the past" ]
                ]
            ]
        , div
            [ class "columns form"
            ]
            [ div
                [ class "column is-one-third" ]
                ((if Set.size shared.eventTypes > 0 then
                    h1 [] [ text "Current types:" ]

                  else
                    span [] []
                 )
                    :: (shared.eventTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , div
                [ class "column is-one-third" ]
                [ div [ class "field" ]
                    [ form
                        [ class "control"
                        , onSubmit <| NewEventType
                        ]
                        [ label
                            [ class "label" ]
                            [ text "Add a new Event type:" ]
                        , input
                            [ type_ "text"
                            , value model.inputEventType
                            , class "input"
                            , onInput InputEventType
                            , placeholder "Enter the name of a new event type"
                            ]
                            [ text "Load default Event Types"
                            ]
                        ]
                    , div [ class "fielset" ]
                        [ label
                            [ class "label" ]
                            [ text "This event type is usable from the following process types:" ]
                        , div [ class "field" ]
                            (shared.processTypes
                                |> Set.toList
                                |> List.sortBy PT.compare
                                |> List.map
                                    (\pt ->
                                        div [ class "control" ]
                                            [ label [ class "checkbox" ]
                                                [ input
                                                    [ type_ "checkbox"
                                                    , onInput InputEventTypeProcessType
                                                    , value pt.name
                                                    , checked (Set.member pt.name model.inputEventTypeProcessTypes)
                                                    ]
                                                    []
                                                , span [] [ text pt.name ]
                                                ]
                                            ]
                                    )
                            )
                        ]
                    , div [ class "field" ]
                        [ div
                            [ class "control" ]
                            [ button
                                [ class "button is-link"
                                , onClick <| NewEventType
                                ]
                                [ text "Add"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
