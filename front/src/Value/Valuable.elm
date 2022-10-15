module Value.Valuable exposing (withValues)

import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Item.Item exposing (Item)
import Typed.Typed exposing (Typed)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (ValueType, initValues)


withValues : Dict String (Typed t) -> Dict String (Hierarchic h) -> Dict String ValueType -> Dict String Value -> Item i -> Item i
withValues allT allH allVts allVs v =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { v
        | values =
            initValues allT allH allVts v.what Nothing v.uuid
                |> Dict.union (Value.fromUuid v.uuid allVs)
    }
