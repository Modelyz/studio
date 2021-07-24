module Page.Loading exposing (wrapper)

import Browser exposing (Document)
import ES
import Html exposing (Html, a, br, button, div, i, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import Status exposing (Status(..))


type alias Model =
    ES.State


wrapper : Model -> Html Msg -> Html Msg
wrapper model content =
    case model.status of
        Loaded ->
            content

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
