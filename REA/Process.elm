module REA.Process exposing (..)

import Html exposing (Html, div, text, a)
import Html.Attributes exposing (class, src, width, href)
import Msg
import REA.Commitment exposing (Commitment)
import REA.Contract exposing (Contract)
import REA.Event exposing (Event)


-- process type is the rea pattern
type alias ProcessType =
    {}


-- a process is a specific occurence of a process type
type Process =
    Process
        { id: Int
        , name: String
        , contract: Contract
        , commitments: List Commitment
        , events: List Event
        }


view: Process -> Html Msg.Msg
view i =
    div [class "column", class "is-one-quarter"]
        [a [href <| "/process/" ++ (String.fromInt <| (\(Process x) -> x.id) i)]
           [div [class "box"]
                [text <| (\(Process x) -> x.name) i]
           ]
        ]
