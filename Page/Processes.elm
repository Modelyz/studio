module Page.Processes exposing (..)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import REA.Process exposing (Process)
import Page.Process
import Msg


view : List Process -> Html Msg.Msg
view processes =
    div
        [ class "section"
        ]
        [ button
            [ onClick <| Msg.NewProcess
            , class "button"
            ]
            [ text "New pizza sale"
            ]
        , div [class "columns", class "is-multiline"]
              <| List.map Page.Process.viewThumbnail processes
        ]
