module Ident.Identifiable exposing (Identifiable, hierarchicWithIdentifiers, typedWithIdentifiers)

import DictSet as Set exposing (DictSet)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier, fromItem)
import Item.Item as Item exposing (Item, OnlyItem)
import Typed.Typed as Typed exposing (Typed)


type alias Identifiable a =
    -- the shape of an identifiable
    { a | identifiers : DictSet String Identifier }


typedWithIdentifiers : DictSet String Identifier -> DictSet String (Typed (Item a)) -> DictSet String (Identifiable (Typed OnlyItem))
typedWithIdentifiers identifiers items =
    -- enrich the set of items with their identifiers
    Set.map compare
        (\item ->
            { what = item.what, uuid = item.uuid, type_ = item.type_, identifiers = fromItem item identifiers }
        )
        items


hierarchicWithIdentifiers : DictSet String Identifier -> DictSet String (Hierarchic (Item a)) -> DictSet String (Identifiable (Hierarchic OnlyItem))
hierarchicWithIdentifiers identifiers items =
    -- enrich the set of items with their identifiers
    Set.map compare
        (\item ->
            { what = item.what, uuid = item.uuid, parent = item.parent, identifiers = fromItem item identifiers }
        )
        items



-- display : Identifiable OnlyItem ->


compare : Identifiable (Item a) -> String
compare =
    Item.compare
