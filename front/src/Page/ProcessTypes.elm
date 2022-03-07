module Page.ProcessTypes exposing (view)

import Browser exposing (Document)
import DictSet as Set
import Html exposing (Html, button, div, form, h1, input, label, p, span, text)
import Html.Attributes exposing (class, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import REA.ProcessType exposing (ProcessType)
import State exposing (State)


type alias Model =
    State


view : Model -> Document Msg
view model =
    { title = "Process Types"
    , body =
        [ Navbar.view model
        , viewContent model
        ]
    }


viewThumbnail : ProcessType -> Html Msg
viewThumbnail pt =
    div
        [ class "container"
        , style "background" "yellow"
        ]
        [ div
            [ class "box", id pt.name ]
            [ text pt.name
            , button
                [ class "delete is-medium"
                , onClick <| DeleteProcessType pt
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
                    [ text "Process Types"
                    ]
                , p [ class "subtitle" ] [ text "What kind of processes may be created" ]
                ]
            ]
        , div
            [ class "columns form"
            ]
            [ div
                [ class "column is-one-third" ]
                ((if Set.size model.processTypes > 0 then
                    h1 [] [ text "Current types:" ]

                  else
                    span [] []
                 )
                    :: (model.processTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , div
                [ class "column is-one-third" ]
                [ label
                    [ class "label" ]
                    [ text "Add a new Process type:" ]
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
                            , placeholder "Enter the name of a new process type"
                            ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ div
                        [ class "control" ]
                        [ button
                            [ class "button is-link"
                            , onClick <| ProcessTypeChanged model.inputProcessType
                            ]
                            [ text "Add"
                            ]
                        ]
                    ]
                ]
            ]
        ]
