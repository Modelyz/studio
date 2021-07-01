module REA.Process exposing (..)

import Html exposing (Html, div, text, a, br)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
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
        [ div [class "card", class "hscroll", onClick <| Msg.NewCommitment][text "Order Pizza"]
        , div [class "card", class "hscroll", onClick <| Msg.NewCommitment][text "Ask payment"]
        , div [class "card", class "hscroll", onClick <| Msg.NewEvent][text "Receive Cash"]
        , div [class "card", class "hscroll", onClick <| Msg.NewEvent][text "Deliver Pizza"]
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
