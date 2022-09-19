module ProcessType.ProcessType exposing (ProcessType, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias ProcessType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String Identifier
    , groups : Dict String Group
    , display : Dict String String
    }


encode : ProcessType -> Encode.Value
encode pt =
    Encode.object
        [ ( "what", Type.encode pt.what )
        , ( "uuid", Uuid.encode pt.uuid )
        , ( "parent", Maybe.map Uuid.encode pt.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ProcessType
decoder =
    Decode.map6 ProcessType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : ProcessType -> String
compare =
    toString


toString : ProcessType -> String
toString =
    .uuid >> Uuid.toString
