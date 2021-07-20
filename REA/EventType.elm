module REA.EventType exposing (EventType, decoder, encode, new)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid


type EventType
    = EventType
        { name : String
        , uuid : Prng.Uuid.Uuid
        , etype : Maybe EventType
        }


new : Prng.Uuid.Uuid -> EventType
new uuid =
    EventType
        { name = "Sale"
        , uuid = uuid
        , etype = Nothing
        }


encode : EventType -> Json.Encode.Value
encode et =
    let
        rec =
            extract et

        t =
            rec.etype
    in
    Json.Encode.object
        [ ( "name", Json.Encode.string rec.name )
        , ( "uuid", Prng.Uuid.encode rec.uuid )
        , ( "etype"
          , case t of
                Nothing ->
                    Json.Encode.string ""

                Just x ->
                    encode x
          )
        ]


extract : EventType -> { name : String, uuid : Prng.Uuid.Uuid, etype : Maybe EventType }
extract (EventType t) =
    t


construct : String -> Prng.Uuid.Uuid -> Maybe EventType -> EventType
construct name uuid etype =
    EventType { name = name, uuid = uuid, etype = etype }


decoder : Json.Decode.Decoder EventType
decoder =
    Json.Decode.map3 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "etype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decoder))
