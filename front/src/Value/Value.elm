module Value.Value exposing (Value, compare, decoder, encode, fromUuid, toValue)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Expression as Expression exposing (Expression)


type alias Value =
    { what : Type
    , for : Uuid
    , name : String
    , expr : Expression
    }


fromUuid : Uuid -> Dict String Value -> Dict String Value
fromUuid uuid =
    -- TODO remove
    Dict.filter (\_ i -> uuid == i.for)


compare : Value -> String
compare v =
    Type.compare v.what ++ "/" ++ Uuid.toString v.for ++ "/" ++ v.name


toValue : Value -> Result String Float
toValue val =
    -- TODO move to Rational
    Expression.eval val.expr


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
