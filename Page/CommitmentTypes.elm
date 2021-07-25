module Page.CommitmentTypes exposing (..)

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
import REA.CommitmentType as CT exposing (CommitmentType)


type alias Model =
    ES.State


view : Model -> Document Msg
view model =
    { title = "Commitment Types"
    , body =
        [ Navbar.view model
        , Loading.wrapper model (viewContent model)
        ]
    }


viewThumbnail : CommitmentType -> Html Msg
viewThumbnail ct =
    div
        [ class "container"
        , style "background" "yellow"
        ]
        [ div
            [ class "box" ]
            [ text ct.name
            , button
                [ class "delete is-large"
                , onClick <| DeleteCommitmentType ct
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
                    [ text "Commitment Types"
                    ]
                , p [ class "subtitle" ] [ text "What kind of events may be expected to happen in the future" ]
                ]
            ]
        , div
            [ class "columns form"
            ]
            [ div
                [ class "column is-one-third" ]
                ([ if Set.size model.commitmentTypes > 0 then
                    h1 [] [ text "Current types:" ]

                   else
                    span [] []
                 ]
                    ++ (model.commitmentTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , div
                [ class "column is-one-third" ]
                [ label
                    [ class "label" ]
                    [ text "Add a new Commitment type:" ]
                , div [ class "field" ]
                    [ form
                        [ class "control"
                        , onSubmit <| NewCommitmentType model.inputCommitmentType
                        ]
                        [ input
                            [ type_ "text"
                            , value model.inputCommitmentType
                            , class "input"
                            , onInput InputCommitmentType
                            , placeholder "Enter the name of a new commitment type"
                            ]
                            [ text "Load default Commitment Types"
                            ]
                        ]
                    ]
                , div [ class "field" ]
                    [ div
                        [ class "control" ]
                        [ button
                            [ class "button is-link"
                            , onClick <| NewCommitmentType model.inputCommitmentType
                            ]
                            [ text "Add"
                            ]
                        ]
                    ]
                ]
            ]
        ]
