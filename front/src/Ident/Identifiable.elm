module Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers, withIdentifiers)

import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType, initIdentifiers)
import Item.Item exposing (Item)
import Prng.Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Typed exposing (Typed)


tWithIdentifiers : Dict String Identifier -> Dict String (Typed a) -> Dict String (Typed a)
tWithIdentifiers allIds ts =
    -- enrich the set of items with their identifiers as a dict
    -- TODO remove tWithIdentifiers and hWithIdentifiers and replace with just withIdentifiers
    Dict.map (\_ t -> { t | identifiers = allIds |> Dict.filter (\_ id -> t.uuid == id.identifiable) }) ts


hWithIdentifiers : Dict String Identifier -> Dict String (Hierarchic a) -> Dict String (Hierarchic a)
hWithIdentifiers allIds hs =
    -- enrich the set of items with their identifiers as a dict
    Dict.map (\_ h -> { h | identifiers = allIds |> Dict.filter (\_ id -> h.uuid == id.identifiable) }) hs


withIdentifiers : Dict String (Typed t) -> Dict String (Hierarchic h) -> Dict String IdentifierType -> Dict String Identifier -> Item i -> Item i
withIdentifiers allT allH allIdts allIds i =
    -- fill with empty identifiers from identifierTypes, then merge with existing identifiers
    { i
        | identifiers =
            initIdentifiers allT allH allIdts i.what Nothing i.uuid
                |> Dict.union (Identifier.fromUuid i.uuid allIds)
    }
