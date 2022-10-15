module GroupType.GroupType exposing (GroupType, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias GroupType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , groups : Dict String Group
    , display : Dict String String
    }


encode : GroupType -> Encode.Value
encode gt =
    Encode.object
        [ ( "what", Type.encode gt.what )
        , ( "uuid", Uuid.encode gt.uuid )
        , ( "parent", Maybe.map Uuid.encode gt.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder GroupType
decoder =
    Decode.map7 GroupType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
