module Restriction.Restriction exposing (Restriction, compare, decoder, encode)

import Entity.Entity as Entity exposing (Entity)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Represent the link between two entity types, the what restricted by the scope
-- TODO replace with a Typed Link? (The type being the event)


type alias Restriction =
    { what : Entity
    , scope : Entity
    }


compare : Restriction -> String
compare r =
    Entity.compare r.what ++ "_" ++ Entity.compare r.scope


encode : Restriction -> Encode.Value
encode r =
    Encode.object
        [ ( "what", Entity.encode r.what )
        , ( "scope", Entity.encode r.scope )
        ]


decoder : Decoder Restriction
decoder =
    Decode.map2 Restriction
        (Decode.field "what" Entity.decoder)
        (Decode.field "scope" Entity.decoder)
