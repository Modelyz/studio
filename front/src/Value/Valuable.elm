module Value.Valuable exposing (hWithValues, tWithValues, withValues)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Item.Item exposing (Item)
import Typed.Typed exposing (Typed)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (ValueType, initValues)


withValues : Dict String (Typed a) -> Dict String (Hierarchic h) -> Dict String ValueType -> Dict String Value -> Item i -> Item i
withValues allT allH allVts allVs v =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { v
        | values =
            initValues allT allH allVts v.what Nothing v.uuid
                |> Dict.union (Value.fromUuid v.uuid allVs)
    }


tWithValues : Dict String (Typed a) -> Dict String (Hierarchic h) -> Dict String ValueType -> Dict String Value -> Typed a -> Typed a
tWithValues allT allH allVts allVs t =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { t
        | values =
            initValues allT allH allVts t.what (H.find allH t.type_) t.uuid
                |> Dict.union (Value.fromUuid t.uuid allVs)
    }


hWithValues : Dict String (Typed a) -> Dict String (Hierarchic h) -> Dict String ValueType -> Dict String Value -> Hierarchic h -> Hierarchic h
hWithValues allT allH allVts allVs t =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { t
        | values =
            initValues allT allH allVts t.what (Maybe.andThen (H.find allH) t.parent) t.uuid
                |> Dict.union (Value.fromUuid t.uuid allVs)
    }
