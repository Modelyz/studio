module Page.EventTypes exposing (view)

import Browser exposing (Document)
import DictSet as Set
import ES
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Msg exposing (Msg(..))
import Page.Loading as Loading
import Page.Navbar as Navbar
import REA.EventType exposing (EventType)


type alias Model =
    ES.State


view : Model -> Document Msg
view model =
    { title = "Event Types"
    , body =
        [ Navbar.view model
        , Loading.wrapper model (viewContent model)
        ]
    }


viewThumbnail : EventType -> Html Msg
viewThumbnail et =
    div
        [ class "container"
        , style "background" "yellow"
        ]
        [ div
            [ class "box" ]
            [ text et.name
            , button
                [ class "delete is-medium"
                , onClick <| DeleteEventType et
                ]
                []
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
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
                ((if Set.size model.eventTypes > 0 then
                    h1 [] [ text "Current types:" ]

                  else
                    span [] []
                 )
                    :: (model.eventTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , div
                [ class "column is-one-third" ]
                [ label
                    [ class "label" ]
                    [ text "Add a new Event type:" ]
                , div [ class "field" ]
                    [ form
                        [ class "control"
                        , onSubmit <| NewEventType model.inputEventType
                        ]
                        [ input
                            [ type_ "text"
                            , value model.inputEventType
                            , class "input"
                            , onInput InputEventType
                            , placeholder "Enter the name of a new event type"
                            ]
                            [ text "Load default Event Types"
                            ]
                        ]
                    ]
                , div [ class "field" ]
                    [ div
                        [ class "control" ]
                        [ button
                            [ class "button is-link"
                            , onClick <| NewEventType model.inputEventType
                            ]
                            [ text "Add"
                            ]
                        ]
                    ]
                ]
            ]
        ]
