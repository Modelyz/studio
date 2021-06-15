module Instance exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, src, width)
import Msg exposing (..)
import Commitment exposing (..)
import Contract exposing (..)
import Event exposing (..)


type Instance =
    Instance
        { id: Int
        , name: String
        , contract: Contract
        , commitments: List Commitment
        , events: List Event
        }


view: Instance -> Html Msg
view i =
    div [class "column", class "is-one-quarter"]
        [div [class "box"]
             [text <| (\(Instance x) -> x.name) i]
        ]
