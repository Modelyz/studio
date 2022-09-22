module Ident.Identifiable exposing (withIdentifiers)

import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType, initIdentifiers)
import Item.Item exposing (Item)
import Prng.Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Typed exposing (Typed)


withIdentifiers : Dict String (Typed t) -> Dict String (Hierarchic h) -> Dict String IdentifierType -> Dict String Identifier -> Item i -> Item i
withIdentifiers allT allH allIdts allIds i =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { i
        | identifiers =
            initIdentifiers allT allH allIdts i.what Nothing i.uuid
                |> Dict.union (Identifier.fromUuid i.uuid allIds)
    }
