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


initValues : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String ValueType -> Type -> Maybe (Hierarchic h) -> Uuid -> Bool -> Dict String Value
initValues allT allH vts t mh uuid isNew =
    -- build the empty values corresponding to the chosen type, possible user type, and uuid of the added/edited entity
    -- if uuid is a newly generated one, we only have the Agent type t, the selected parent type and No uuid.
    -- So we must find all the vt whose scope is ascendent of (HasUserType t h.what h.uuid).
    vts
        |> Dict.filter
            (\_ vt ->
                if isNew then
                    Maybe.map (\h -> Scope.containsScope allT allH (HasUserType t h.what h.uuid) vt.scope) mh |> Maybe.withDefault False

                else
                    Scope.containsScope allT allH (IsItem t uuid) vt.scope
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
    -- TODO replace "|" with "/" since its percentEncoded in the URL?
    Scope.compare vt.scope ++ "|" ++ vt.name


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
