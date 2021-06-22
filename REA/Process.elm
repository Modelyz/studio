module REA.Process exposing (..)

import Html exposing (Html, div, text, a)
import Html.Attributes exposing (class, href)
import Json.Encode
import Msg
import Prng.Uuid

import REA
import REA.Contract

view : REA.Process -> Html Msg.Msg
view p =
    div [class "column", class "is-one-quarter"]
        [a [href <| "/process/" ++ (Prng.Uuid.toString p.uuid)]
           [div [class "box"]
                [text p.name]
           ]
        ]

new : Prng.Uuid.Uuid -> REA.Process
new uuid =
    { uuid=uuid
    , name="Pizza sale"
    , contract=REA.Contract.new
--    , commitments=[]
--    , events=[]
    }

encode : REA.Process -> Json.Encode.Value
encode p =
    Json.Encode.object
        [ ("uuid", Json.Encode.string <| Prng.Uuid.toString p.uuid)
        , ("name", Json.Encode.string p.name)
        , ("contract", REA.Contract.encode p.contract)
        ]
    
