module Group.Link exposing (Link, compare, decoder, encode)

import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Link =
    -- the groupable is in the group
    { groupable : Groupable
    , group : Group
    }


encode : Link -> Value
encode eg =
    Encode.object
        [ ( "groupable", Groupable.encode eg.groupable )
        , ( "group", Group.encode eg.group )
        ]


decoder : Decoder Link
decoder =
    Decode.map2 Link
        (Decode.field "groupable" Groupable.decoder)
        (Decode.field "group" Group.decoder)


compare : Link -> String
compare eg =
    Groupable.compare eg.groupable ++ Group.compare eg.group
