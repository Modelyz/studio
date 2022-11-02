module ResourceType.ResourceType exposing (ResourceType, compare, decoder, encode)

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


type alias ResourceType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , groups : Dict String Group
    , display : Dict String String
    }


encode : ResourceType -> Encode.Value
encode at =
    Encode.object <|
        [ ( "what", HType.encode at.what )
        , ( "uuid", Uuid.encode at.uuid )
        , ( "parent", Maybe.map Uuid.encode at.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ResourceType
decoder =
    Decode.map7 ResourceType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : ResourceType -> String
compare =
    toString


toString : ResourceType -> String
toString =
    .uuid >> Uuid.toString
