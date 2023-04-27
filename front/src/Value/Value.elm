module Value.Value exposing (Value, compare, decoder, encode, fromUuid, getByUuid)

import Dict exposing (Dict)
import Expression exposing (Expression)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Value =
    { what : Type
    , for : Uuid
    , name : String
    , expr : Expression

    -- TODO!! important:
    --, value : Result String Rational
    --, date: Posix
    -- Ability to store the computed value, and trigger new values
    }


compare : Value -> String
compare v =
    Type.compare v.what ++ "/" ++ Uuid.toString v.for ++ "/" ++ v.name


fromUuid : Uuid -> Dict String Value -> Dict String Value
fromUuid uuid =
    -- TODO rename to filterUuid
    Dict.filter (\_ v -> uuid == v.for)


getByUuid : Uuid -> Dict String Value -> Result String Value
getByUuid uuid all =
    fromUuid uuid all
        |> Dict.values
        |> List.head
        |> Result.fromMaybe "Missing value"


encode : Value -> Encode.Value
encode v =
    Encode.object
        [ ( "what", Type.encode v.what )
        , ( "for", Uuid.encode v.for )
        , ( "name", Encode.string v.name )
        , ( "expr", Expression.encode v.expr )
        ]


decoder : Decoder Value
decoder =
    Decode.map4 Value
        (Decode.field "what" Type.decoder)
        (Decode.field "for" Uuid.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "expr" Expression.decoder)
