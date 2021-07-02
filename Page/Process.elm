module Page.Process exposing (..)

import Html exposing (Html, div, text, a, br)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Msg
import Prng.Uuid

import REA.Process exposing (Process)

viewThumbnail : Process -> Html Msg.Msg
viewThumbnail p =
    div [ class "column", class "is-one-quarter"]
        [ a [ href <| "/process/" ++ (Prng.Uuid.toString p.uuid)]
            [div [ class "box"]
                [ text <| p.name
                , br [] []
                , text <| Prng.Uuid.toString p.uuid
                ]
            ]
        ]

viewFullpage : Process -> Html Msg.Msg
viewFullpage p =
    div [class "section", class "hscroll-container"]
        [ div [class "button", class "hscroll", onClick <| Msg.NewCommitment][text "Order Pizza"]
        , div [class "button", class "hscroll", onClick <| Msg.NewCommitment][text "Ask payment"]
        , div [class "button", class "hscroll", onClick <| Msg.NewEvent][text "Receive Cash"]
        , div [class "button", class "hscroll", onClick <| Msg.NewEvent][text "Deliver Pizza"]
        ]
