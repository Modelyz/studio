module REA.EventType exposing (..)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid

import REA


new: Prng.Uuid.Uuid -> REA.EventType
new uuid=
    REA.EventType
    { name="Sale"
    , uuid=uuid
    , etype=Nothing
    }


encode : REA.EventType -> Json.Encode.Value
encode et =
    let
        rec = extract et
        t = rec.etype
    in
    Json.Encode.object
        [ ("name", Json.Encode.string rec.name)
        , ("uuid", Prng.Uuid.encode rec.uuid)
        , ("etype",
            case t of
                Nothing -> Json.Encode.string ""
                Just x -> encode x)
        ]

extract (REA.EventType t) = t


construct : String -> Prng.Uuid.Uuid -> Maybe REA.EventType -> REA.EventType
construct name uuid etype =
    REA.EventType { name=name, uuid=uuid, etype=etype }


decode : Json.Decode.Decoder REA.EventType
decode =
    Json.Decode.map3 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "etype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decode))

