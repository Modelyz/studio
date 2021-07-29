module Page.ProcessType exposing (view)

import Browser exposing (Document)
import DictSet as Set
import ES
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Msg exposing (Msg(..))
import Page.Loading as Loading
import Page.Navbar as Navbar
import REA.ProcessType exposing (ProcessType)


type alias Model =
    ES.State


view : Model -> Document Msg
view model =
    { title = "Process Type"
    , body =
        [ Navbar.view model
        , Loading.wrapper model (viewContent model)
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
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
                            , value model.inputProcessType.processName
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
                                (if model.inputProcessType == model.processType then
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
