module Value.ValueType exposing (ValueType, compare, decoder, encode, initValues)

import Dict exposing (Dict)
import Expression exposing (Expression)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Type exposing (Type)
import Value.Value as Value exposing (Value)


type alias ValueType =
    -- this is the definition of a value field
    { name : String
    , expr : Expression
    , scope : Scope
    , mandatory : Bool
    }


initValues : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String ValueType -> Type -> Maybe Uuid -> Uuid -> Bool -> Dict String Value
initValues types vts t muuid uuid isNew =
    -- build the empty values corresponding to the chosen type, possible user type, and uuid of the added/edited entity
    -- if uuid is a newly generated one, we only have the Agent type t, the selected parent type and No uuid.
    -- So we must find all the vt whose scope is ascendent of (HasUserType t h.uuid).
    vts
        |> Dict.filter
            (\_ vt ->
                if isNew then
                    containsScope types
                        (Maybe.map (HasUserType t) muuid
                            |> Maybe.withDefault (HasType t)
                        )
                        vt.scope

                else
                    containsScope types (IsItem t uuid) vt.scope
            )
        |> Dict.values
        |> List.map
            (\vt ->
                let
                    i =
                        Value t uuid vt.name vt.expr
                in
                ( Value.compare i, i )
            )
        |> Dict.fromList


compare : ValueType -> String
compare vt =
    Scope.compare vt.scope ++ "/" ++ vt.name


encode : ValueType -> Encode.Value
encode vt =
    Encode.object
        [ ( "name", Encode.string vt.name )
        , ( "expr", Expression.encode vt.expr )
        , ( "scope", Scope.encode vt.scope )
        , ( "mandatory", Encode.bool vt.mandatory )
        ]


decoder : Decoder ValueType
decoder =
    Decode.map4 ValueType
        (Decode.field "name" Decode.string)
        (Decode.field "expr" Expression.decoder)
        (Decode.field "scope" Scope.decoder)
        (Decode.field "mandatory" Decode.bool)
