module Value.ValueType exposing (ValueType, compare, decoder, encode, initValues)

import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Typed exposing (Typed)
import Value.Value as Value exposing (..)


type alias ValueType =
    -- this is the definition of a value field
    { name : String
    , expr : Expression
    , scope : Scope
    , mandatory : Bool
    }


initValues : Dict String (Typed t) -> Dict String (Hierarchic b) -> Dict String ValueType -> Type -> Maybe (Hierarchic h) -> Uuid -> Dict String Value
initValues allT allH vts t mh newUuid =
    -- build the empty values corresponding to the chosen type and possible user type
    let
        hscope =
            -- scope corresponding to the hierarchic entity we're dealing with
            Maybe.map (\h -> HasUserType t h.uuid) mh |> Maybe.withDefault (HasType t)
    in
    vts
        |> Dict.filter (\_ vt -> Scope.containsScope allT allH hscope vt.scope)
        |> Dict.values
        |> List.map
            (\vt ->
                let
                    v =
                        Value t newUuid vt.name vt.expr
                in
                ( Value.compare v, v )
            )
        |> Dict.fromList


compare : ValueType -> String
compare vt =
    Scope.compare vt.scope ++ "/" ++ vt.name


encode : ValueType -> Encode.Value
encode vt =
    Encode.object
        [ ( "name", Encode.string vt.name )
        , ( "expr", eEncode vt.expr )
        , ( "scope", Scope.encode vt.scope )
        , ( "mandatory", Encode.bool vt.mandatory )
        ]


decoder : Decoder ValueType
decoder =
    Decode.map4 ValueType
        (Decode.field "name" Decode.string)
        (Decode.field "expr" eDecoder)
        (Decode.field "scope" Scope.decoder)
        (Decode.field "mandatory" Decode.bool)
