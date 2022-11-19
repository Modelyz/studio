module CommitmentType.CommitmentType exposing (CommitmentType, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Type as HType
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias CommitmentType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , providers : Scope
    , receivers : Scope
    , flow : Scope
    , groups : Dict String Group
    , display : Dict String String
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object <|
        [ ( "what", HType.encode ct.what )
        , ( "uuid", Uuid.encode ct.uuid )
        , ( "parent", Maybe.map Uuid.encode ct.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder CommitmentType
decoder =
    Decode.map6 (\wh uu pa pr re fl -> CommitmentType wh uu pa Dict.empty Dict.empty pr re fl Dict.empty Dict.empty)
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.field "providers" Scope.decoder)
        (Decode.field "receivers" Scope.decoder)
        (Decode.field "flow" Scope.decoder)


compare : CommitmentType -> String
compare =
    toString


toString : CommitmentType -> String
toString =
    .uuid >> Uuid.toString
