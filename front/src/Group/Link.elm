module Group.Link exposing (Link, compare, decoder, encode, groupsOf)

import Dict exposing (Dict)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Item.Item exposing (Item)
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


groupsOf : Dict String Link -> Item a -> Dict String Group
groupsOf gs i =
    gs
        |> Dict.filter (\_ v -> i.uuid == Groupable.uuid v.groupable)
        |> Dict.map (\_ v -> v.group)
