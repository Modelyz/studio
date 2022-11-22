module Value.Valuable exposing (getValues)

import Dict exposing (Dict)
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (ValueType, initValues)


getValues : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String ValueType -> Dict String Value -> Type -> Uuid -> Maybe Uuid -> Bool -> Dict String Value
getValues types allVts allVs t uuid mpuuid isNew =
    -- start with empty identifiers from identifierTypes, then merge with existing identifiers
    initValues types allVts t mpuuid uuid isNew
        |> Dict.union (Value.fromUuid uuid allVs)
