module Value.Valuable exposing (getValues)

import Dict exposing (Dict)
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (ValueType, initValues)


getValues : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String ValueType -> Dict String Value -> Type -> Uuid -> Dict String Value
getValues types allVts allVs t uuid =
    -- start with empty identifiers from identifierTypes, then merge with existing identifiers
    initValues types allVts t (Just uuid) uuid False
        |> Dict.union (Value.fromUuid uuid allVs)
