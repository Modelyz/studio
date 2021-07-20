module REA.AgentType exposing (AgentType, decoder, encode)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid


type AgentType
    = AgentType
        { name : String
        , uuid : Prng.Uuid.Uuid
        , atype : Maybe AgentType
        }


encode : AgentType -> Json.Encode.Value
encode at =
    let
        rec =
            extract at

        t =
            rec.atype
    in
    Json.Encode.object
        [ ( "name", Json.Encode.string rec.name )
        , ( "atype"
          , case t of
                Nothing ->
                    Json.Encode.string ""

                Just x ->
                    encode x
          )
        ]


extract : AgentType -> { name : String, uuid : Prng.Uuid.Uuid, atype : Maybe AgentType }
extract (AgentType t) =
    t


construct : String -> Prng.Uuid.Uuid -> Maybe AgentType -> AgentType
construct name uuid atype =
    AgentType { name = name, uuid = uuid, atype = atype }


decoder : Json.Decode.Decoder AgentType
decoder =
    Json.Decode.map3 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "atype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decoder))
