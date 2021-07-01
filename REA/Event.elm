module REA.Event exposing (..)

import Json.Decode
import Json.Encode
import REA
import REA.EventType
import Prng.Uuid


new: Prng.Uuid.Uuid -> REA.Event
new uuid=
    { name="Pizza order"
    , uuid=uuid
    , etype=REA.EventType.new uuid -- FIXME select the type
    }


encode : REA.Event -> Json.Encode.Value
encode e =
    Json.Encode.object
        [ ("name", Json.Encode.string e.name)
        , ("uuid", Prng.Uuid.encode e.uuid)
        , ("etype", REA.EventType.encode e.etype)
        ]


decode : Json.Decode.Decoder REA.Event
decode =
    Json.Decode.map3 REA.Event
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "etype" REA.EventType.decode)
        

entity : REA.Event -> Json.Decode.Decoder REA.Entity
entity event = Json.Decode.succeed <| REA.EVENT event

