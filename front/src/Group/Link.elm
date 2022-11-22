module Group.Link exposing (Link, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Link =
    -- the groupable is in the group
    { what : Type
    , groupable : Uuid
    , group : Uuid
    }


compare : Link -> String
compare link =
    Uuid.toString link.groupable ++ ":" ++ Uuid.toString link.group


encode : Link -> Encode.Value
encode link =
    Encode.object
        [ ( "type", Type.encode link.what )
        , ( "groupable", Uuid.encode link.groupable )
        , ( "group", Uuid.encode link.group )
        ]


decoder : Decoder Link
decoder =
    Decode.map3 Link
        (Decode.field "type" Type.decoder)
        (Decode.field "groupable" Uuid.decoder)
        (Decode.field "group" Uuid.decoder)
