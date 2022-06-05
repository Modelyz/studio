module REA.Restriction exposing (Restriction, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import REA.EntityType as ENT exposing (EntityType)



-- Represent the link between two entity types, the what restricted by the scope
-- TODO replace with a Typed Link? (The type being the event)


type alias Restriction =
    { what : EntityType
    , scope : EntityType
    }


compare : Restriction -> String
compare r =
    ENT.compare r.what ++ "_" ++ ENT.compare r.scope


encode : Restriction -> Encode.Value
encode r =
    Encode.object
        [ ( "what", ENT.encode r.what )
        , ( "scope", ENT.encode r.scope )
        ]


decoder : Decoder Restriction
decoder =
    Decode.map2 Restriction
        (Decode.field "what" ENT.decoder)
        (Decode.field "scope" ENT.decoder)
