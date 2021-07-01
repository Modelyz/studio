module REA.Process exposing (..)

import Html exposing (Html, div, text, a, br)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Msg
import Prng.Uuid

import REA.Commitment as Cm exposing (Commitment)
import REA.Contract as C exposing (Contract)
import REA.Event as E exposing (Event)

-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process
type alias Process =
        { uuid: Prng.Uuid.Uuid
        , name: String
        , contracts: List Contract
        , commitments: List Commitment
        , events: List Event
        }


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


new : Prng.Uuid.Uuid -> Process
new uuid =
    { uuid=uuid
    , name="Pizza sale"
    , contracts=[C.new]
    , commitments=[]
    , events=[]
--    , fullfilments=[]
    }

encode : Process -> Json.Encode.Value
encode p =
    Json.Encode.object
        [ ("uuid", Prng.Uuid.encode p.uuid)
        , ("name", Json.Encode.string p.name)
        , ("contracts", Json.Encode.list C.encode p.contracts)
        , ("commitments", Json.Encode.list Cm.encode p.commitments)
        , ("events", Json.Encode.list E.encode p.events)
        ]


--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decode : Json.Decode.Decoder Process
decode =
    Json.Decode.map5 Process
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "contracts" <| Json.Decode.list C.decode)
        (Json.Decode.field "commitments" <| Json.Decode.list Cm.decode)
        (Json.Decode.field "events" <| Json.Decode.list E.decode)
