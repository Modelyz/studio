module Page.ProcessType exposing (view)

import Browser exposing (Document)
import DictSet as Set
import ES
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Maybe exposing (andThen)
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import REA.ProcessType exposing (ProcessType)
import Route


type alias Model =
    ES.State


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
                                (if model.inputProcessType == ptype then
                                    True

                                 else
                                    False
                                )
                            , onClick <| ProcessTypeChanged model.inputProcessType
                            ]
                            [ text "Change"
                            ]
                        ]
                    ]
                ]
            ]
        ]
