module Page.EventTypes exposing (..)

import Browser exposing (Document)
import DictSet as Set
import ES
import Html exposing (Attribute, Html, a, br, button, div, form, h1, h2, i, img, input, label, nav, p, span, text)
import Html.Attributes exposing (attribute, class, href, placeholder, src, style, type_, value, width)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Json.Decode as Decode
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
                [ class "delete is-large"
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
                ([ if Set.size model.eventTypes > 0 then
                    h1 [] [ text "Current types:" ]

                   else
                    span [] []
                 ]
                    ++ (model.eventTypes
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
