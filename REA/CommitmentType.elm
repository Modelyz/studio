module REA.CommitmentType exposing (CommitmentType, decode, encode, new)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid


type CommitmentType
    = CommitmentType
        { name : String
        , uuid : Prng.Uuid.Uuid
        , ctype : Maybe CommitmentType
        }


new : Prng.Uuid.Uuid -> CommitmentType
new uuid =
    CommitmentType
        { name = "Order"
        , uuid = uuid
        , ctype = Nothing
        }


encode : CommitmentType -> Json.Encode.Value
encode ct =
    let
        rec =
            extract ct

        t =
            rec.ctype
    in
    Json.Encode.object
        [ ( "name", Json.Encode.string rec.name )
        , ( "uuid", Prng.Uuid.encode rec.uuid )
        , ( "ctype"
          , case t of
                Nothing ->
                    Json.Encode.string ""

                Just x ->
                    encode x
          )
        ]


extract : CommitmentType -> { name : String, uuid : Prng.Uuid.Uuid, ctype : Maybe CommitmentType }
extract (CommitmentType t) =
    t


construct : String -> Prng.Uuid.Uuid -> Maybe CommitmentType -> CommitmentType
construct name uuid ctype =
    CommitmentType { name = name, uuid = uuid, ctype = ctype }


decode : Json.Decode.Decoder CommitmentType
decode =
    Json.Decode.map3 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "ctype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decode))
