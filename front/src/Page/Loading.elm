module Page.Loading exposing (wrapper)

import Browser exposing (Document)
import ES
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Status exposing (ESStatus(..))


type alias Model =
    ES.State


wrapper : Model -> Html Msg -> Html Msg
wrapper model content =
    case model.esstatus of
        ESIdle ->
            content

        ESReading ->
            div [ class "section" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-spinner fa-pulse" ]
                            []
                        ]
                    ]
                , span [] [ text " Reading from IDB..." ]
                ]

        ESError error ->
            div [ class "section" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-bug" ]
                            []
                        ]
                    ]
                , span [] [ text <| " Error : " ++ error ]
                ]

        ESStoring ->
            div [ class "section" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-spinner fa-pulse" ]
                            []
                        ]
                    ]
                , span [] [ text <| " Storing..." ]
                ]
