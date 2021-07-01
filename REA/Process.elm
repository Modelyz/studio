module REA.Process exposing (..)

import Html exposing (Html, div, text, a, br)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Msg
import Prng.Uuid

import REA
import REA.Commitment
import REA.Contract
import REA.Event

viewThumbnail : REA.Process -> Html Msg.Msg
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

viewFullpage : REA.Process -> Html Msg.Msg
viewFullpage p =
    div [class "section", class "hscroll-container"]
        [ div [class "button", class "hscroll", onClick <| Msg.NewCommitment][text "Order Pizza"]
        , div [class "button", class "hscroll", onClick <| Msg.NewCommitment][text "Ask payment"]
        , div [class "button", class "hscroll", onClick <| Msg.NewEvent][text "Receive Cash"]
        , div [class "button", class "hscroll", onClick <| Msg.NewEvent][text "Deliver Pizza"]
        ]


new : Prng.Uuid.Uuid -> REA.Process
new uuid =
    { uuid=uuid
    , name="Pizza sale"
    , contracts=[REA.Contract.new]
    , commitments=[]
    , events=[]
--    , fullfilments=[]
    }

encode : REA.Process -> Json.Encode.Value
encode p =
    Json.Encode.object
        [ ("uuid", Prng.Uuid.encode p.uuid)
        , ("name", Json.Encode.string p.name)
        , ("contracts", Json.Encode.list REA.Contract.encode p.contracts)
        , ("commitments", Json.Encode.list REA.Commitment.encode p.commitments)
        , ("events", Json.Encode.list REA.Event.encode p.events)
        ]


--merge : REA.Process -> REA.Process -> REA.Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decode : Json.Decode.Decoder REA.Process
decode =
    Json.Decode.map5 REA.Process
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "contracts" <| Json.Decode.list REA.Contract.decode)
        (Json.Decode.field "commitments" <| Json.Decode.list REA.Commitment.decode)
        (Json.Decode.field "events" <| Json.Decode.list REA.Event.decode)


entity : REA.Process -> Json.Decode.Decoder REA.Entity
entity process = Json.Decode.succeed <| REA.PROCESS process

