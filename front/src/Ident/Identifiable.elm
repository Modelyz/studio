module Ident.Identifiable exposing (gWithIdentifiers, hWithIdentifiers, tWithIdentifiers, withIdentifiers)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType, initIdentifiers)
import Item.Item exposing (Item)
import Prng.Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import State exposing (State)
import Type exposing (Type)
import Typed.Typed exposing (Typed)



-- TODO move to somewhere else (see the same for Value) and remove this module
-- TODO or restore a separated Identifiable type?


gWithIdentifiers : Dict String Group -> Dict String (Hierarchic b) -> Dict String IdentifierType -> Dict String Identifier -> Group -> Group
gWithIdentifiers allT allH allIdts allIds i =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { i
        | identifiers =
            initIdentifiers allT allH allIdts (Type.TType i.what) Nothing i.uuid
                |> Dict.union (Identifier.fromUuid i.uuid allIds)
    }


tWithIdentifiers : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String IdentifierType -> Dict String Identifier -> Typed a -> Typed a
tWithIdentifiers allT allH allIdts allIds i =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { i
        | identifiers =
            initIdentifiers allT allH allIdts (Type.TType i.what) (H.find allH i.type_) i.uuid
                |> Dict.union (Identifier.fromUuid i.uuid allIds)
    }


hWithIdentifiers : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String IdentifierType -> Dict String Identifier -> Hierarchic b -> Hierarchic b
hWithIdentifiers allT allH allIdts allIds i =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { i
        | identifiers =
            initIdentifiers allT allH allIdts (Type.HType i.what) Nothing i.uuid
                |> Dict.union (Identifier.fromUuid i.uuid allIds)
    }


withIdentifiers : State -> Dict String { a | uuid : Uuid, identifiers : Dict String Identifier } -> Dict String { a | uuid : Uuid, identifiers : Dict String Identifier }
withIdentifiers state =
    Dict.map (\_ x -> { x | identifiers = state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })
