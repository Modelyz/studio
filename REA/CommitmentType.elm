module REA.CommitmentType exposing (..)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid

import REA


new : Prng.Uuid.Uuid -> REA.CommitmentType
new uuid=
    REA.CommitmentType
    { name="Order"
    , uuid=uuid
    , ctype=Nothing
    }


encode : REA.CommitmentType -> Json.Encode.Value
encode ct =
    let
        rec = extract ct
        t = rec.ctype
    in
    Json.Encode.object
        [ ("name", Json.Encode.string rec.name)
        , ("uuid", Prng.Uuid.encode rec.uuid)
        , ("ctype",
            case t of
                Nothing -> Json.Encode.string ""
                Just x -> encode x)
        ]

extract (REA.CommitmentType t) = t

construct : String -> Prng.Uuid.Uuid -> Maybe REA.CommitmentType -> REA.CommitmentType
construct name uuid ctype =
    REA.CommitmentType { name=name, uuid=uuid, ctype=ctype }


decode : Json.Decode.Decoder REA.CommitmentType
decode =
    Json.Decode.map3 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "ctype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decode))

entity : REA.CommitmentType -> Json.Decode.Decoder REA.Entity
entity commitmentType = Json.Decode.succeed <| REA.COMMITMENTTYPE commitmentType

