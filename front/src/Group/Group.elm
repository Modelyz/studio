module Group.Group exposing (Group, decoder, encode, groupsOf)

import Dict exposing (Dict)
import Group.Link as GroupLink
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope)
import Typed.Type as TType


type alias Group =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    , parent : Maybe Uuid

    -- The scope defines what a group can contain
    , scope : Scope
    }


groupsOf : Dict String GroupLink.Link -> Uuid -> List Uuid
groupsOf links uuid =
    links
        |> Dict.filter (\_ link -> link.groupable == uuid)
        |> Dict.values
        |> List.map .group


encode : Group -> Encode.Value
encode g =
    Encode.object
        ([ ( "what", TType.encode g.what )
         , ( "uuid", Uuid.encode g.uuid )
         , ( "type", Uuid.encode g.type_ )
         , ( "scope", Scope.encode g.scope )
         ]
            ++ (g.parent |> Maybe.map (\parent -> [ ( "parent", Uuid.encode parent ) ]) |> Maybe.withDefault [])
        )


decoder : Decoder Group
decoder =
    Decode.map5 Group
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "parent" Uuid.decoder))
        (Decode.field "scope" Scope.decoder)
