module Group.EntityGroup exposing (EntityGroup, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias EntityGroup =
    -- the entity is in the group
    { entity : Uuid
    , group : Uuid
    }


encode : EntityGroup -> Value
encode eg =
    Encode.object
        [ ( "entity", Uuid.encode eg.entity )
        , ( "group", Uuid.encode eg.group )
        ]


decoder : Decoder EntityGroup
decoder =
    Decode.map2 EntityGroup
        (Decode.field "entity" Uuid.decoder)
        (Decode.field "group" Uuid.decoder)


compare : EntityGroup -> String
compare eg =
    Uuid.toString eg.entity ++ Uuid.toString eg.group
