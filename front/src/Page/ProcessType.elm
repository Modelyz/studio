module Page.ProcessType exposing (view)

import Browser exposing (Document)
import Html exposing (Html, button, div, form, input, label, p, text)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import REA.ProcessType exposing (ProcessType)
import State exposing (State)


type alias Model =
    State


view : Model -> ProcessType -> Document Msg
view model ptype =
    { title = "Process Type"
    , body =
        [ Navbar.view model
        , viewContent model ptype
        ]
    }


viewContent : Model -> ProcessType -> Html Msg
viewContent model ptype =
    div
        []
        [ div
            [ class "hero is-medium"
            ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text "Process Type"
                    ]
                , p [ class "subtitle" ] [ text "Configuration of the type of processes managed by this service" ]
                ]
            ]
        , div
            [ class "columns form"
            ]
            [ div
                [ class "column is-one-third" ]
                [ label
                    [ class "label" ]
                    [ text "Process name:" ]
                , div [ class "field" ]
                    [ form
                        [ class "control"
                        , onSubmit <| ProcessTypeChanged model.inputProcessType
                        ]
                        [ input
                            [ type_ "text"
                            , value model.inputProcessType.name
                            , class "input"
                            , onInput InputProcessName
                            , placeholder "Enter the name of the processes to create"
                            ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ div
                        [ class "control" ]
                        [ button
                            [ class "button is-link"
                            , disabled
                                (model.inputProcessType == ptype)
                            , onClick <| ProcessTypeChanged model.inputProcessType
                            ]
                            [ text "Change"
                            ]
                        ]
                    ]
                ]
            ]
        ]
