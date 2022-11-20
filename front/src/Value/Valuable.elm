module Value.Valuable exposing (getValues, withValues)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Item.Item exposing (Item)
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Typed exposing (Typed)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (ValueType, initValues)


withValues : Dict String ( Type, Maybe Uuid ) -> Dict String ValueType -> Dict String Value -> Item i -> Item i
withValues types allVts allVs v =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { v
        | values =
            initValues types allVts v.what Nothing v.uuid False
                |> Dict.union (Value.fromUuid v.uuid allVs)
    }


getValues : Dict String ( Type, Maybe Uuid ) -> Dict String ValueType -> Dict String Value -> Type -> Uuid -> Dict String Value
getValues types allVts allVs t uuid =
    -- start with empty identifiers from identifierTypes, then merge with existing identifiers
    initValues types allVts t (Just uuid) uuid False
        |> Dict.union (Value.fromUuid uuid allVs)
