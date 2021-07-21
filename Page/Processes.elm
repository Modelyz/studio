module Page.Processes exposing (Model, view)

import Browser exposing (Document)
import DictSet
import ES
import Html exposing (Html, a, br, button, div, i, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Prng.Uuid as Uuid
import REA.Process as P exposing (Process)
import Status exposing (Status(..))


type alias Model =
    ES.State


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


view : Model -> Document Msg
view model =
    { title = "Processes"
    , body =
        [ viewNavbar
        , viewContent model
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div
        [ class "section"
        ]
        [ button
            [ onClick NewProcess
            , class "button"
            ]
            [ text "New pizza sale"
            ]
        , case model.status of
            Loaded ->
                div [ class "columns is-multiline" ]
                    (DictSet.toList model.processes
                        |> List.sortBy P.compare
                        |> List.reverse
                        |> List.map viewThumbnail
                    )

            Loading ->
                div [ class "section" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-spinner fa-pulse" ]
                                []
                            ]
                        ]
                    , span [] [ text " Loading..." ]
                    ]

            Failed error ->
                div [ class "section" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-bug" ]
                                []
                            ]
                        ]
                    , span [] [ text <| " Error : " ++ error ]
                    ]
        ]


viewThumbnail : Process -> Html Msg
viewThumbnail p =
    div [ class "column is-one-quarter" ]
        [ a [ href <| "/process/" ++ Uuid.toString p.uuid ]
            [ div [ class "box" ]
                [ text <| p.name
                , br [] []
                , text <| Uuid.toString p.uuid
                ]
            ]
        ]
