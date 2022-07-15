module Group.Group exposing (Group, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Group =
    { uuid : Uuid
    , type_ : String

    -- TODO how to define what is in the group?
    }


encode : Group -> Value
encode g =
    Encode.object
        [ ( "uuid", Uuid.encode g.uuid )
        , ( "type", Encode.string g.type_ )
        ]


decoder : Decoder Group
decoder =
    Decode.map2 Group
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Decode.string)


compare : Group -> String
compare =
    .uuid >> Uuid.toString
