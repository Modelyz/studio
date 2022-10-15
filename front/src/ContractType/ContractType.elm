module ContractType.ContractType exposing (ContractType, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias ContractType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , groups : Dict String Group
    , display : Dict String String
    }


encode : ContractType -> Encode.Value
encode ct =
    Encode.object
        [ ( "what", Type.encode ct.what )
        , ( "uuid", Uuid.encode ct.uuid )
        , ( "parent", Maybe.map Uuid.encode ct.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ContractType
decoder =
    Decode.map7 ContractType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : ContractType -> String
compare =
    toString


toString : ContractType -> String
toString =
    .uuid >> Uuid.toString
