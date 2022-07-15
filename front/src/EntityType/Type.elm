module EntityType.Type exposing (Type, decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Type =
    { name : String
    , parent : Maybe String
    }


encode : Type -> Encode.Value
encode t =
    Encode.object
        [ ( "name", Encode.string t.name )
        , ( "parent", Maybe.map Encode.string t.parent |> Maybe.withDefault Encode.null )
        ]


decode : Decoder Type
decode =
    Decode.map2
        Type
        (Decode.field "name" Decode.string)
        (Decode.field "parent" <| Decode.maybe Decode.string)
