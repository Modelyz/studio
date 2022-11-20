module EventType.EventType exposing (EventType, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Type as HType
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias EventType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


encode : EventType -> Encode.Value
encode et =
    Encode.object <|
        [ ( "what", HType.encode et.what )
        , ( "uuid", Uuid.encode et.uuid )
        , ( "parent", Maybe.map Uuid.encode et.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder EventType
decoder =
    Decode.map3 EventType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)


compare : EventType -> String
compare =
    toString


toString : EventType -> String
toString =
    .uuid >> Uuid.toString
